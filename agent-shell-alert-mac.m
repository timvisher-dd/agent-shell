/* agent-shell-alert-mac.m -- Emacs dynamic module for macOS native notifications.
 *
 * Provides three notification functions:
 *
 * 1. agent-shell-alert-mac-notify -- UNUserNotificationCenter (preferred).
 *    See: https://developer.apple.com/documentation/usernotifications/unusernotificationcenter
 *    KNOWN ISSUE: This path currently fails with UNErrorDomain error 1
 *    (UNErrorCodeNotificationsNotAllowed) on the Homebrew emacs-app cask
 *    build.  An adhoc-signed Emacs built from source works fine.  Apple
 *    docs say no entitlement is needed for local notifications, and the
 *    hardened runtime has no notification-related restrictions, so the
 *    root cause is unknown.  See x.notification-center-spiking.md for
 *    the full investigation.  The Elisp layer detects this failure at
 *    load time and falls back to the AppleScript path below.
 *
 * 2. agent-shell-alert-mac-applescript-notify -- NSAppleScript fallback.
 *    Runs `display notification` from within Emacs's process so macOS
 *    attributes the notification to Emacs (icon, click-to-activate).
 *    This is the current working path for GUI Emacs on macOS.  It uses
 *    the deprecated AppleScript notification bridge but works on current
 *    macOS versions.
 *
 * 3. agent-shell-alert-mac-request-authorization -- requests notification
 *    permission via UNUserNotificationCenter.  Called at load time; if it
 *    fails, the Elisp layer switches to the AppleScript path.
 *
 * Build: cc -Wall -O2 -fPIC -shared -fobjc-arc \
 *          -I<emacs-include-dir> \
 *          -framework UserNotifications -framework Foundation \
 *          -o agent-shell-alert-mac.dylib agent-shell-alert-mac.m
 */

#include <string.h>
#include <stdlib.h>
#include <emacs-module.h>

#import <Foundation/Foundation.h>
#import <UserNotifications/UserNotifications.h>

/* Required by Emacs module API: GPL compatibility declaration. */
int plugin_is_GPL_compatible;

/* --- Helpers --- */

/* Extract a C string from an Emacs string value.  Caller must free(). */
static char *
extract_string(emacs_env *env, emacs_value val)
{
  ptrdiff_t len = 0;
  env->copy_string_contents(env, val, NULL, &len);
  if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
    return NULL;

  char *buf = malloc(len);
  if (!buf)
    return NULL;

  env->copy_string_contents(env, val, buf, &len);
  if (env->non_local_exit_check(env) != emacs_funcall_exit_return) {
    free(buf);
    return NULL;
  }
  return buf;
}

/* Signal an Emacs error with a message string. */
static void
signal_error(emacs_env *env, const char *msg)
{
  emacs_value sym = env->intern(env, "error");
  emacs_value data = env->make_string(env, msg, strlen(msg));
  env->non_local_exit_signal(env, sym, data);
}

/* Pump the NSRunLoop until done becomes YES or timeout seconds elapse. */
static void
run_loop_until(BOOL *done, NSTimeInterval timeout)
{
  NSDate *limit = [NSDate dateWithTimeIntervalSinceNow:timeout];
  while (!*done && [[NSDate date] compare:limit] == NSOrderedAscending)
    [[NSRunLoop currentRunLoop] runMode:NSDefaultRunLoopMode
                             beforeDate:[NSDate dateWithTimeIntervalSinceNow:0.1]];
}

/* --- Notification operations --- */

/* (agent-shell-alert-mac-notify TITLE BODY)
 * Posts a macOS notification.  Returns t on success, nil on failure. */
static emacs_value
Fagent_shell_alert_mac_notify(emacs_env *env, ptrdiff_t nargs,
                              emacs_value *args, void *data)
{
  (void)nargs;
  (void)data;

  char *title_c = extract_string(env, args[0]);
  if (!title_c)
    return env->intern(env, "nil");
  char *body_c = extract_string(env, args[1]);
  if (!body_c) {
    free(title_c);
    return env->intern(env, "nil");
  }

  NSString *title = [NSString stringWithUTF8String:title_c];
  NSString *body = [NSString stringWithUTF8String:body_c];
  free(title_c);
  free(body_c);

  UNUserNotificationCenter *center =
      [UNUserNotificationCenter currentNotificationCenter];

  if (!center) {
    signal_error(env,
                 "agent-shell-alert-mac-notify: "
                 "UNUserNotificationCenter unavailable "
                 "(no bundle identifier?)");
    return env->intern(env, "nil");
  }

  UNMutableNotificationContent *content =
      [[UNMutableNotificationContent alloc] init];
  content.title = title;
  content.body = body;
  content.sound = [UNNotificationSound defaultSound];

  NSString *identifier =
      [NSString stringWithFormat:@"agent-shell-%f",
                                 [[NSDate date] timeIntervalSince1970]];
  UNNotificationRequest *request =
      [UNNotificationRequest requestWithIdentifier:identifier
                                           content:content
                                           trigger:nil];

  __block BOOL done = NO;
  __block NSString *err_desc = nil;

  [center addNotificationRequest:request
           withCompletionHandler:^(NSError *error) {
             if (error)
               err_desc = [[error localizedDescription] copy];
             done = YES;
           }];

  run_loop_until(&done, 10.0);

  if (err_desc) {
    char msg[512];
    snprintf(msg, sizeof(msg),
             "agent-shell-alert-mac-notify: %s", [err_desc UTF8String]);
    signal_error(env, msg);
    return env->intern(env, "nil");
  }
  if (!done) {
    signal_error(env, "agent-shell-alert-mac-notify: timed out");
    return env->intern(env, "nil");
  }

  return env->intern(env, "t");
}

/* (agent-shell-alert-mac-request-authorization)
 * Requests notification authorization.  Returns t if granted. */
static emacs_value
Fagent_shell_alert_mac_request_authorization(emacs_env *env, ptrdiff_t nargs,
                                             emacs_value *args, void *data)
{
  (void)nargs;
  (void)args;
  (void)data;

  UNUserNotificationCenter *center =
      [UNUserNotificationCenter currentNotificationCenter];

  if (!center) {
    signal_error(env,
                 "agent-shell-alert-mac-request-authorization: "
                 "UNUserNotificationCenter unavailable "
                 "(no bundle identifier?)");
    return env->intern(env, "nil");
  }

  __block BOOL done = NO;
  __block BOOL granted = NO;
  __block NSString *err_desc = nil;

  [center requestAuthorizationWithOptions:(UNAuthorizationOptionAlert |
                                           UNAuthorizationOptionSound)
                        completionHandler:^(BOOL g, NSError *error) {
                          granted = g;
                          if (error)
                            err_desc = [[error localizedDescription] copy];
                          done = YES;
                        }];

  run_loop_until(&done, 30.0);

  if (err_desc) {
    char msg[512];
    snprintf(msg, sizeof(msg),
             "agent-shell-alert-mac-request-authorization: %s",
             [err_desc UTF8String]);
    signal_error(env, msg);
    return env->intern(env, "nil");
  }
  if (!done) {
    signal_error(env,
                 "agent-shell-alert-mac-request-authorization: timed out");
    return env->intern(env, "nil");
  }

  return env->intern(env, granted ? "t" : "nil");
}

/* (agent-shell-alert-mac-applescript-notify TITLE BODY)
 * Posts a notification via NSAppleScript from within Emacs's process,
 * so macOS attributes it to Emacs (icon, click-to-activate).
 * Does not require UNUserNotificationCenter entitlements. */
static emacs_value
Fagent_shell_alert_mac_applescript_notify(emacs_env *env, ptrdiff_t nargs,
                                          emacs_value *args, void *data)
{
  (void)nargs;
  (void)data;

  char *title_c = extract_string(env, args[0]);
  if (!title_c)
    return env->intern(env, "nil");
  char *body_c = extract_string(env, args[1]);
  if (!body_c) {
    free(title_c);
    return env->intern(env, "nil");
  }

  NSString *script =
      [NSString stringWithFormat:
                    @"display notification %@ with title %@",
                    [NSString stringWithFormat:@"\"%@\"",
                              [[NSString stringWithUTF8String:body_c]
                                  stringByReplacingOccurrencesOfString:@"\""
                                                           withString:@"\\\""]],
                    [NSString stringWithFormat:@"\"%@\"",
                              [[NSString stringWithUTF8String:title_c]
                                  stringByReplacingOccurrencesOfString:@"\""
                                                           withString:@"\\\""]]];
  free(title_c);
  free(body_c);

  NSDictionary *error = nil;
  NSAppleScript *as = [[NSAppleScript alloc] initWithSource:script];
  [as executeAndReturnError:&error];

  if (error) {
    NSString *desc = error[NSAppleScriptErrorMessage]
                         ?: @"unknown AppleScript error";
    char msg[512];
    snprintf(msg, sizeof(msg),
             "agent-shell-alert-mac-applescript-notify: %s",
             [desc UTF8String]);
    signal_error(env, msg);
    return env->intern(env, "nil");
  }

  return env->intern(env, "t");
}

/* --- Module initialization --- */

static void
bind_function(emacs_env *env, const char *name, emacs_value func)
{
  emacs_value sym = env->intern(env, name);
  emacs_value args[] = {sym, func};
  env->funcall(env, env->intern(env, "defalias"), 2, args);
}

int
emacs_module_init(struct emacs_runtime *runtime)
{
  emacs_env *env = runtime->get_environment(runtime);

  if ((size_t)env->size < sizeof(*env))
    return 1;

  bind_function(
      env, "agent-shell-alert-mac-notify",
      env->make_function(
          env, 2, 2, Fagent_shell_alert_mac_notify,
          "Post a macOS native notification.\n\n"
          "(agent-shell-alert-mac-notify TITLE BODY)\n\n"
          "Uses UNUserNotificationCenter.  Returns t on success.",
          NULL));

  bind_function(
      env, "agent-shell-alert-mac-request-authorization",
      env->make_function(
          env, 0, 0, Fagent_shell_alert_mac_request_authorization,
          "Request macOS notification authorization.\n\n"
          "(agent-shell-alert-mac-request-authorization)\n\n"
          "Call once to prompt the user for notification permission.\n"
          "Returns t if granted, nil otherwise.",
          NULL));

  bind_function(
      env, "agent-shell-alert-mac-applescript-notify",
      env->make_function(
          env, 2, 2, Fagent_shell_alert_mac_applescript_notify,
          "Post a notification via AppleScript from Emacs's process.\n\n"
          "(agent-shell-alert-mac-applescript-notify TITLE BODY)\n\n"
          "Uses NSAppleScript so the notification is attributed to Emacs.\n"
          "Does not require UNUserNotificationCenter entitlements.",
          NULL));

  emacs_value feature = env->intern(env, "agent-shell-alert-mac");
  emacs_value provide_args[] = {feature};
  env->funcall(env, env->intern(env, "provide"), 1, provide_args);

  return 0;
}
