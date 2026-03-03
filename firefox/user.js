
/*
   ==========================================================
    GENERAL
   ==========================================================
*/

// Use autoscrolling
user_pref("general.autoScroll", true);

// Always show the Bookmarks Toolbar
user_pref("browser.toolbars.bookmarks.visibility", "always");


/* 
   ==========================================================
    HOME
   ========================================================== 
*/

// Homepage and new windows: Blank Page
user_pref("browser.startup.homepage", "chrome://browser/content/blanktab.html");

// Restore previous session
user_pref("browser.startup.page", 1);

// New tabs: Disable Firefox Home / custom New Tab page
user_pref("browser.newtabpage.enabled", false);

// Firefox Home content
user_pref("browser.newtabpage.activity-stream.feeds.topsites", false);
user_pref("browser.newtabpage.activity-stream.section.highlights.includeBookmarks", false);
user_pref("browser.newtabpage.activity-stream.section.highlights.includeDownloads", false);
user_pref("browser.newtabpage.activity-stream.section.highlights.includeVisited", false);
user_pref("browser.newtabpage.activity-stream.showSearch", false);
user_pref("browser.newtabpage.activity-stream.showSponsoredTopSites", false);
user_pref("browser.newtabpage.activity-stream.showSponsoredCheckboxes", false);


/*
   ==========================================================
    SEARCH
   ========================================================== 
*/

// Show search terms instead of URL in the address bar
user_pref("browser.urlbar.showSearchTerms.enabled", false);

// Address bar suggestions
user_pref("browser.urlbar.suggest.history", false);
user_pref("browser.urlbar.suggest.bookmark", false);
user_pref("browser.urlbar.suggest.openpage", false);
user_pref("browser.urlbar.suggest.engines", false);
user_pref("browser.urlbar.suggest.topsites", false);
user_pref("browser.urlbar.suggest.trending", false);
user_pref("browser.urlbar.suggest.recentsearches", false);
user_pref("browser.urlbar.suggest.quickactions", false);


/* 
   ==========================================================
    FILES AND APPLICATIONS
   ========================================================== 
*/

// Always ask you where to save files
user_pref("browser.download.always_ask_before_handling_new_types", true);


/*
   ==========================================================
    PRIVACY & SECURITY
   ========================================================== 
*/

// Enhanced Tracking Protection: Standard
user_pref("browser.contentblocking.category", "standard");

// Send websites a "Do Not Track" / Global Privacy Control signal
user_pref("privacy.globalprivacycontrol.enabled", true);

// History: Use custom settings for history
user_pref("privacy.history.custom", true);

// Never remember history
user_pref("places.history.enabled", false);

// Clear history when Firefox closes
user_pref("privacy.sanitize.sanitizeOnShutdown", true);

// Settings for clearing history on close
user_pref("privacy.clearOnShutdown_v2.formdata", true);
user_pref("privacy.clearOnShutdown_v2.siteSettings", true);
user_pref("privacy.clearOnShutdown_v2.cookiesAndStorage", false);

// Forms and Autofill
user_pref("browser.formfill.enable", false);
user_pref("extensions.formautofill.addresses.enabled", false);
user_pref("extensions.formautofill.creditCards.enabled", false);

// Logins and Passwords
user_pref("signon.rememberSignons", false);
user_pref("signon.autofillForms", false);
user_pref("signon.generation.enabled", false);


/* 
   ==========================================================
    BROWSER UI
   ==========================================================
*/

// Show tab preview on hover
user_pref("browser.tabs.hoverPreview.showThumbnails", false);


/* 
   ==========================================================
    PDF
   ========================================================== 
*/

// Open PDFs in Firefox
user_pref("pdfjs.enabledCache.state", true);


/* 
   ==========================================================
    VIDEO
   ========================================================== 
*/

// Enable Picture-in-Picture video controls
user_pref("media.videocontrols.picture-in-picture.video-toggle.enabled", false);
