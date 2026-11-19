// Browser Behavior
// Settings that control how the browser behaves during startup, shutdown, and general use.
user_pref("browser.aboutConfig.showWarning", false);           // Disables the warning displayed when accessing about:config.
user_pref("browser.download.useDownloadDir", false);           // Forces Firefox to prompt for a download location instead of using the default directory.
user_pref("browser.search.suggest.enabled", true);             // [true while kagi] Disables search suggestions to prevent query leaks.
user_pref("browser.shell.checkDefaultBrowser", false);         // Prevents Firefox from checking if it is the default browser on startup.
user_pref("browser.startup.homepage", "about:blank");          // Sets the homepage to a blank page for a clean startup experience.
user_pref("browser.startup.page", 0);                          // Configures Firefox to start with a blank page (0 = blank, 1 = homepage, 3 = resume previous session).
user_pref("browser.tabs.closeWindowWithLastTab", false);       // Prevents closing the window when the last tab is closed.
user_pref("browser.tabs.loadBookmarksInTabs", false);          // Open bookmarks in new tabs by default
user_pref("browser.toolbars.bookmarks.visibility", "always");  // Always shows the bookmarks toolbar.")
user_pref("browser.translations.panelShown", false);           // Don't show the translation panel.
user_pref("browser.urlbar.suggest.searches", true);            // [true while kagi] Disables search suggestions in the URL bar for privacy.
user_pref("browser.warnOnQuit", false);                        // Disables the warning prompt when quitting Firefox.
user_pref("browser.warnOnQuitShortcut", false);                // Disables the warning when quitting Firefox using a keyboard shortcut.
user_pref("media.autoplay.default", 5);                        // Sets autoplay to block all media (5 = block all).

// Crash Reporting
// Settings related to crash report collection and submission.
user_pref("breakpad.reportURL", "");                           // Disables crash reporting by clearing the report submission URL.
user_pref("browser.crashReports.unsubmittedCheck.autoSubmit2", false); // Prevents automatic submission of unsubmitted crash reports.
user_pref("browser.tabs.crashReporting.sendReport", false);    // Disables sending crash reports when a tab crashes.

// Data Reporting and Telemetry
// Settings to disable telemetry and data reporting features.
user_pref("app.normandy.api_url", "");                         // Disables Normandy server interactions by clearing its API URL.
user_pref("app.normandy.enabled", false);                      // Turns off Normandy, which delivers experiments and updates.
user_pref("app.shield.optoutstudies.enabled", false);          // Disables participation in Shield studies.
user_pref("browser.newtabpage.activity-stream.feeds.telemetry", false); // Disables telemetry for new tab page feeds.
user_pref("browser.newtabpage.activity-stream.telemetry", false); // Disables telemetry for the activity stream on the new tab page.
user_pref("datareporting.healthreport.uploadEnabled", false);  // Prevents upload of Firefox Health Report data.
user_pref("datareporting.policy.dataSubmissionEnabled", false); // Disables data submission for policy reporting.
user_pref("datareporting.usage.uploadEnabled", false);         // Turns off usage data reporting.

// Extensions and Add-ons
// Settings to manage extension behavior and recommendations.
user_pref("extensions.getAddons.showPane", false);             // Hides the add-ons discovery pane in the add-ons manager.
user_pref("extensions.htmlaboutaddons.recommendations.enabled", false); // Disables add-on recommendations in about:addons.
user_pref("extensions.pocket.enabled", false);                 // Disables the Pocket extension built into Firefox.
user_pref("extensions.openPopupWithoutUserGesture.enabled", true); // Required for Obsidian to work properly.

// Form and Autofill Management
// Settings to control form autofill and password management features.
user_pref("extensions.formautofill.addresses.enabled", false); // Disables address autofill in forms.
user_pref("extensions.formautofill.creditCards.enabled", false); // Disables credit card autofill in forms.
user_pref("signon.autofillForms", false);                      // Prevents autofilling of login forms.
user_pref("signon.firefoxRelay.feature", "disabled");          // Disables Firefox Relay integration for email masking.
user_pref("signon.generation.enabled", false);                 // Disables automatic password generation.
user_pref("signon.management.page.breach-alerts.enabled", false); // Turns off breach alerts in the password manager.
user_pref("signon.rememberSignons", false);                    // Disables remembering login credentials.

// Network and Connectivity
// Settings affecting network behavior, DNS, and WebRTC configurations.
user_pref("captivedetect.canonicalURL", "");                   // Disables captive portal detection by clearing its URL.
user_pref("dom.security.https_only_mode", true);               // Enforces HTTPS-only mode for all connections.
user_pref("geo.enabled", false);                               // Disables geolocation services to protect user location data.
user_pref("geo.provider.use_corelocation", false);             // Disables the use of Core Location for geolocation services.
user_pref("media.peerconnection.ice.default_address_only", true); // Limits WebRTC to the default network interface for privacy.
user_pref("media.peerconnection.ice.proxy_only_if_behind_proxy", true); // Forces WebRTC to use the proxy if behind one.
// user_pref("media.peerconnection.ice.no_host", true);        // Optionally excludes private IPs from WebRTC (commented out as it may break some sites).
user_pref("network.captive-portal-service.enabled", false);    // Turns off the captive portal detection service.
user_pref("network.connectivity-service.enabled", false);      // Disables the network connectivity service.
user_pref("network.http.referer.XOriginTrimmingPolicy", 2);    // Reduces cross-origin referer info to scheme+host+port (2 = trim to origin).
user_pref("network.trr.mode", 3);                              // Sets DNS over HTTPS (TRR) to maximum enforcement (3 = TRR only).
user_pref("network.trr.uri", "https://doh.dns.sb/dns-query"); // Specifies DNS.SB's DNS over HTTPS provider.

// New Tab Page Customization
// Settings to customize the new tab page experience.
user_pref("browser.newtabpage.activity-stream.default.sites", ""); // Clears default pinned sites on the new tab page.
user_pref("browser.newtabpage.activity-stream.showSponsored", false); // Disables sponsored content on the new tab page.
user_pref("browser.newtabpage.activity-stream.showSponsoredTopSites", false); // Prevents sponsored top sites from appearing.
user_pref("browser.newtabpage.enabled", false);                 // set new tab page to blank.

// Privacy Enhancements
// Settings to improve user privacy and limit tracking.
user_pref("browser.contentblocking.category", "strict");       // Sets content blocking to strict mode for enhanced tracking protection.
user_pref("privacy.annotate_channels.strict_list.enabled", true); // Enables strict tracking protection list for annotated channels.
user_pref("privacy.bounceTrackingProtection.mode", 1);         // Activates bounce tracking protection (1 = enabled).
user_pref("privacy.fingerprintingProtection", true);           // Enables protection against browser fingerprinting.
user_pref("privacy.globalprivacycontrol.enabled", true);       // Signals privacy preferences to websites via Global Privacy Control.
user_pref("privacy.query_stripping.enabled", true);            // Strips tracking parameters from URLs.
user_pref("privacy.query_stripping.enabled.pbmode", true);     // Activates query stripping in private browsing mode.
user_pref("privacy.sanitize.sanitizeOnShutdown", false);        // Don't sanitizes data on shutdown.
user_pref("privacy.trackingprotection.emailtracking.enabled", true); // Protects against email tracking.
user_pref("privacy.trackingprotection.enabled", true);         // Enables general tracking protection.
user_pref("privacy.trackingprotection.socialtracking.enabled", true); // Protects against social media tracking.

// User Interface Customization
// Settings to tweak the browser’s UI and behavior.
user_pref("browser.ml.enable", false);                         // Disable AI stuff
user_pref("browser.ml.chat.enabled", false);
user_pref("browser.display.background_color.dark", "#292c33"); // Sets a custom dark background color for internal pages.
user_pref("browser.engagement.sidebar-button.has-used", true); // Marks the sidebar button as used to avoid prompts.
user_pref("sidebar.main.tools", "history");          // Configures sidebar tools to show bookmarks and history.
user_pref("sidebar.revamp", true);                             // Enables the revamped sidebar UI.
user_pref("sidebar.revamp.round-content-area", true);          // Rounds the content area in the sidebar.
user_pref("sidebar.verticalTabs", true);                       // Activates vertical tabs in the sidebar.
user_pref("sidebar.visibility", "always-show");                // Always shows the sidebar.
user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true); // Allows custom userChrome.css and userContent.css styles.

