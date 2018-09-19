#!/usr/bin/env bash

# ~/.osx — http://mths.be/osx
# Watch for changes in files with either of
#  sudo fs_usage | grep plist
#  sudo opensnoop | grep plist

if [[ $# -ne 1 ]];then
  echo "Usage: ./$(basename "$0") COMPNAME"
  exit
fi

killall System\ Preferences

# Ask for the administrator password upfront
sudo -v

# Keep-alive: update existing 'sudo' time stamp until '.osx' has finished
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

# Set computer name
name=$1
sudo scutil --set ComputerName "$name"
sudo scutil --set HostName "$name.local"
sudo scutil --set LocalHostName "$name"
sudo defaults write /Library/Preferences/SystemConfiguration/com.apple.smb.server NetBIOSName -string "$name"


#
# General Settings
#

# Default sidebar icon size to small
defaults write NSGlobalDomain NSTableViewDefaultSizeMode -int 1

# Ask to keep changes on close
defaults write NSGlobalDomain NSCloseAlwaysConfirmsChanges -int 1

# Disables shutting down inactive applications
defaults write NSGlobalDomain NSDisableAutomaticTermination -bool true

# Disable the "reopen windows when logging back in" option
# This works, although the checkbox will still appear to be checked.
defaults write com.apple.loginwindow TALLogoutSavesState -bool false
defaults write com.apple.loginwindow LoginwindowLaunchesRelaunchApps -bool false
# Disable reopen on restart
defaults write NSGlobalDomain ApplePersistence -bool false

# Disabling App Nap
#defaults write NSGlobalDomain NSAppSleepDisabled -bool YES

# Always show scrollbars
defaults write NSGlobalDomain AppleShowScrollBars -string "Always"

# Dark UI
defaults write NSGlobalDomain AppleInterfaceStyle -string "Dark"

# Disable Resume system-wide
defaults write com.apple.systempreferences NSQuitAlwaysKeepsWindows -bool false

# Enable subpixel font rendering on non-Apple LCDs
defaults write NSGlobalDomain AppleFontSmoothing -int 2

# Disable 'smart' quotes
defaults write NSGlobalDomain NSAutomaticQuoteSubstitutionEnabled -bool false

# Disable smart dashes
defaults write NSGlobalDomain NSAutomaticDashSubstitutionEnabled -bool false

# Use tabs
defaults write NSGlobalDomain AppleWindowTabbingMode -string "always"


#
# Desktop & Screen Saver
#

# Disable the screen saver (System Preferences must be closed)
defaults -currentHost write com.apple.screensaver idleTime -int 0

# Ask for password for screensaver
defaults write com.apple.screensaver askForPassword -int 1

# Set delay before password ask (minutes x 60)
defaults write com.apple.screensaver askForPasswordDelay -int 5

# Reveal IP address, hostname, OS version, etc. when clicking the clock in the login window
sudo defaults write /Library/Preferences/com.apple.loginwindow AdminHostInfo HostName

# Set login window text
sudo defaults write /Library/Preferences/com.apple.loginwindow LoginwindowText -string "I'm sorry Dave, I'm afraid I can't do that"


#
# Dock
#

# Set the icon size of Dock items to 36 pixels
defaults write com.apple.dock tilesize -int 36

# Disable dock magification
defaults write com.apple.dock magnification -bool false

# Put the dock on left side
defaults write com.apple.dock orientation -string "left"

# Automatically hide and show the Dock
defaults write com.apple.dock autohide -bool true

# Change the auto-hiding Dock delay
defaults write com.apple.dock autohide-delay -float 0.0
defaults write com.apple.dock autohide-time-modifier -float 0.0

# Show indicator lights for open applications in the Dock
defaults write com.apple.dock show-process-indicators -bool true

# Make Dock icons of hidden applications translucent
defaults write com.apple.dock showhidden -bool true

# Dock tweaks
defaults write com.apple.dock mouse-over-hilte-stack -bool true


#
# Mission Control
#

# Don't show Dashboard as a Space
defaults write com.apple.dock dashboard-in-overlay -bool true

# Disable Dashboard
defaults write com.apple.dashboard mcx-disabled -bool true

# Don't automatically rearrange Spaces based on most recent use
defaults write com.apple.dock mru-spaces -bool false

# Dock setup
if command -v dockutil; then
  dockutil --remove all

  dockutil --add "/Applications/Firefox.app"
  dockutil --add "/Applications/Tweetbot.app"
  dockutil --add "/Applications/Messages.app"
  dockutil --add "/Applications/Xcode.app"
  dockutil --add "/Applications/Utilities/Terminal.app"

  dockutil --add "/Applications" --view list --display folder --sort name
  dockutil --add "$HOME/Dropbox" --view grid --display folder --sort name
  dockutil --add "$HOME/Downloads" --view grid --display stack --sort dateadded
else
  echo "dockutil not installed, re-run after installing"
fi

# Run hot corners script
if [[ -f ../bin/corners ]]; then
    ../bin/corners enable
else
    echo "Failed to setup hot corners, script missing"
fi

#
# Spotlight
#

# Disable Spotlight
# sudo launchctl unload -w /System/Library/LaunchDaemons/com.apple.metadata.mds.plist
# sudo mdutil -a -i off

# Hide Spotlight Icon
# sudo mv /System/Library/CoreServices/Search.bundle /System/Library/CoreServices/Search.bundle.bak

# Show Spotlight Icon
#sudo mv /System/Library/CoreServices/Search.bundle.bak /System/Library/CoreServices/Search.bundle
#killall SystemUIServer


#
# Terminal
#

# Disable leading [ on prompt lines (which is totally broken in anything curses)
# https://twitter.com/UINT_MIN/status/652142001932996609
defaults write com.apple.Terminal AutoMarkPromptLines -bool false
defaults write com.apple.Terminal ShowLineMarks -bool false

# Hide scrollbars in terminal
defaults write com.apple.Terminal AppleShowScrollBars -string "Automatic"

# Setup the correct theme
defaults write com.apple.Terminal "Default Window Settings" -string "parsec"
defaults write com.apple.Terminal "Startup Window Settings" -string "parsec"


#
# Firewall
#

# Enable Firewall. Possible values:
# 0 = off
# 1 = on for specific services
# 2 = on for essential services
sudo defaults write /Library/Preferences/com.apple.alf globalstate -int 1
# Enable Stealth mode.
sudo defaults write /Library/Preferences/com.apple.alf stealthenabled -int 1
# Enable Firewall logging.
sudo defaults write /Library/Preferences/com.apple.alf loggingenabled -int 1

# Disable IR remote control.
sudo defaults write /Library/Preferences/com.apple.driver.AppleIRController DeviceEnabled -bool false

# Disable auto-adjust brightness
sudo defaults write /Library/Preferences/com.apple.iokit.AmbientLightSensor.plist "Automatic Display Enabled" -bool false


#
# Media
#

# Disable all actions when inserting disks
defaults write com.apple.digihub com.apple.digihub.blank.bd.appeared -dict-add action -int 1
defaults write com.apple.digihub com.apple.digihub.blank.cd.appeared -dict-add action -int 1
defaults write com.apple.digihub com.apple.digihub.blank.dvd.appeared -dict-add action -int 1
defaults write com.apple.digihub com.apple.digihub.cd.music.appeared -dict-add action -int 1
defaults write com.apple.digihub com.apple.digihub.dvcamera.IIDC.appeared -dict-add action -int 1
defaults write com.apple.digihub com.apple.digihub.dvcamera.IIDC.irisopened -dict-add action -int 1
defaults write com.apple.digihub com.apple.digihub.dvd.video.appeared -dict-add action -int 1


#
# Displays
#

# Show displays in menu bar
defaults write com.apple.airplay showInMenuBarIfPresent -bool false

# Enable HiDPI display modes (requires restart)
sudo defaults write /Library/Preferences/com.apple.windowserver DisplayResolutionEnabled -bool true


#
# Power Settings
#

# To stop the display from half dimming before full display 'sleep'
# http://developer.apple.com/library/mac/#documentation/Darwin/Reference/ManPages/man1/pmset.1.html
sudo pmset -a halfdim 0

# Sleep options
sudo pmset -a displaysleep 5
sudo pmset -a sleep 0
sudo pmset -a disksleep 0

# Wake for network access
sudo pmset -a womp 1

# Don't restart after power failure
sudo pmset -a autorestart 0

# Wake computer when laptop is opened
sudo pmset -a lidwake 1

# Don't wake computer when power source changes
sudo pmset -a acwake 0

# Don't dim brightness on any different source
sudo pmset -a lessbright 0

# Disable sudden motion sensor
sudo pmset -a sms 0

# Disable startup sound
sudo nvram SystemAudioVolume=" "

# Enable the MacBook Air SuperDrive on any Mac
sudo nvram boot-args="mbasd=1"

# Power button behavior
defaults write com.apple.loginwindow PowerButtonSleepsSystem -bool NO


#
# Keyboard
#

# Disable press-and-hold for keys in favor of key repeat
defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false

# Set a blazingly fast keyboard repeat rate
defaults write NSGlobalDomain KeyRepeat -int 2
defaults write NSGlobalDomain InitialKeyRepeat -int 15

# Disable auto-correct
defaults write NSGlobalDomain NSAutomaticSpellingCorrectionEnabled -bool false
# Disable auto capitalize
defaults write NSGlobalDomain NSAutomaticCapitalizationEnabled -bool false
# Disable auto period insert
defaults write NSGlobalDomain NSAutomaticPeriodSubstitutionEnabled -bool false


#
# Mouse/Trackpad
#

# Trackpad: enable tap to click for this user and for the login screen
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true
defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1

# Trackpad: disable three finger tap
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadThreeFingerTapGesture -int 0
defaults -currentHost write NSGlobalDomain com.apple.trackpad.threeFingerTapGesture -int 0

# Trackpad: disable two finger pinch
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadPinch -int 0
defaults -currentHost write NSGlobalDomain com.apple.trackpad.pinchGesture -int 0

# Trackpad: 'smart zoom' two finger double tap
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadTwoFingerDoubleTapGesture -int 0
defaults -currentHost write NSGlobalDomain com.apple.trackpad.twoFingerDoubleTapGesture -int 0

# Trackpad: disable trackpad rotate
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadRotate -int 0
defaults -currentHost write NSGlobalDomain com.apple.trackpad.rotateGesture -int 0

# Trackpad: disable swipe from right to show notification center
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadTwoFingerFromRightEdgeSwipeGesture -int 0
defaults -currentHost write NSGlobalDomain com.apple.trackpad.twoFingerFromRightEdgeSwipeGesture -int 0

# Mouse: TwoButton mouse
defaults write com.apple.driver.AppleBluetoothMultitouch.mouse.plist MouseButtonMode -string "TwoButton"

# Turn Bluetooth off.
sudo defaults write /Library/Preferences/com.apple.Bluetooth ControllerPowerState -int 0

# Disable El Capitan shake to magnify cursor
defaults write NSGlobalDomain CGDisableCursorLocationMagnification -bool true


#
# Sound
#

# Change the error sound
defaults write com.apple.systemsound com.apple.sound.beep.sound -string "/System/Library/Sounds/Morse.aiff"


#
# Date/Time
#

# Setup the menu bar date format
defaults write com.apple.menuextra.clock DateFormat -string "EEE MMM d  h:mm a"

# Flash the : in the menu bar
defaults write com.apple.menuextra.clock FlashDateSeparators -bool false

# 24 hour time
defaults write NSGlobalDomain AppleICUForce24HourTime -bool true
defaults write NSGlobalDomain AppleICUTimeFormatStrings -dict \
  1 -string "H:mm" \
  2 -string "H:mm:ss" \
  3 -string "H:mm:ss z" \
  4 -string "H:mm:ss zzzz"


#
# Battery Percentage
#

defaults write com.apple.menuextra.battery ShowPercent -bool true


#
# Finder
#

# Show all icons on desktop
defaults write com.apple.finder ShowExternalHardDrivesOnDesktop -bool true
defaults write com.apple.finder ShowHardDrivesOnDesktop -bool true
defaults write com.apple.finder ShowMountedServersOnDesktop -bool true
defaults write com.apple.finder ShowRemovableMediaOnDesktop -bool true

# Finder: disable window animations and Get Info animations
defaults write com.apple.finder DisableAllAnimations -bool true

# Finder: show all filename extensions
defaults write NSGlobalDomain AppleShowAllExtensions -bool true

# Show Status bar in Finder
defaults write com.apple.finder ShowStatusBar -bool true

# Show Path bar in Finder
defaults write com.apple.finder ShowPathbar -bool true

# Use column view in all Finder windows by default
# Four-letter codes for the other view modes: 'icnv', 'clmv', 'Flwv', 'Nlsv'
defaults write com.apple.finder FXPreferredViewStyle -string clmv

# Allow text selection in QuickLook
defaults write com.apple.finder QLEnableTextSelection -bool true

# Display full POSIX path as Finder window title
defaults write com.apple.finder _FXShowPosixPathInTitle -bool true

# When performing a search, search the current folder by default
defaults write com.apple.finder FXDefaultSearchScope -string SCcf

# Finder: new window location set to $HOME. Same as Finder > Preferences > New Finder Windows show
# For other path use "PfLo" and "file:///foo/bar/"
defaults write com.apple.finder NewWindowTarget -string PfLo
defaults write com.apple.finder NewWindowTargetPath -string "file://$HOME/"

# Disable the warning when changing a file extension
defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false

# Default to local files instead of iCloud
defaults write NSGlobalDomain NSDocumentSaveNewDocumentsToCloud -bool false

# Disable the warning before emptying the Trash
defaults write com.apple.finder WarnOnEmptyTrash -bool false

# Empty Trash securely by default
# defaults write com.apple.finder EmptyTrashSecurely -bool true

# Expand the following File Info panes:
# “General”, “Open with”, and “Sharing & Permissions”
defaults write com.apple.finder FXInfoPanesExpanded -dict \
	General -bool true \
	OpenWith -bool true \
	Privileges -bool true

# Should remove downloaded from the internet warnings
defaults write com.apple.LaunchServices LSQuarantine -bool false

# Enable highlight hover effect for the grid view of a stack (Dock)
defaults write com.apple.dock mouse-over-hilite-stack -bool true

# Show the ~/Library folder
chflags nohidden ~/Library

# Automatically open a new Finder window when a volume is mounted
defaults write com.apple.frameworks.diskimages auto-open-ro-root -bool true
defaults write com.apple.frameworks.diskimages auto-open-rw-root -bool true
defaults write com.apple.finder OpenWindowForNewRemovableDisk -bool true

# Don't use tabs in Finder
defaults write com.apple.finder AppleWindowTabbingMode -string "manual"

# Enable snap-to-grid for icons on the desktop and in other icon views
/usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:arrangeBy grid" ~/Library/Preferences/com.apple.finder.plist

# Show item info near icons on the desktop and in other icon views
/usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:showItemInfo true" ~/Library/Preferences/com.apple.finder.plist

# Increase grid spacing for icons on the desktop and in other icon views
/usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:gridSpacing 64" ~/Library/Preferences/com.apple.finder.plist

# Increase the size of icons on the desktop and in other icon views
/usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:iconSize 64" ~/Library/Preferences/com.apple.finder.plist

# Set the finder window toolbar to only have back/forward buttons
/usr/libexec/PlistBuddy -c "Delete :NSToolbar\\ Configuration\\ Browser:TB\\ Item\\ Identifiers" ~/Library/Preferences/com.apple.finder.plist
/usr/libexec/PlistBuddy -c "Add :NSToolbar\\ Configuration\\ Browser:TB\\ Item\\ Identifiers array" ~/Library/Preferences/com.apple.finder.plist
/usr/libexec/PlistBuddy -c "Add :NSToolbar\\ Configuration\\ Browser:TB\\ Item\\ Identifiers:0 string com.apple.finder.BACK" ~/Library/Preferences/com.apple.finder.plist

# Remove all tags from contextual menu
/usr/libexec/PlistBuddy -c "Delete :FavoriteTagNames" ~/Library/Preferences/com.apple.finder.plist
/usr/libexec/PlistBuddy -c "Add :FavoriteTagNames array" ~/Library/Preferences/com.apple.finder.plist
/usr/libexec/PlistBuddy -c "Add :FavoriteTagNames:0 string" ~/Library/Preferences/com.apple.finder.plist


#
# Safari/WebKit
#

# Change the Safari search to find strings contained in other words
defaults write com.apple.Safari FindOnPageMatchesWordStartsOnly -bool false

# Show developer tools
defaults write com.apple.Safari IncludeDevelopMenu -bool true
defaults write com.apple.Safari WebKitDeveloperExtrasEnabledPreferenceKey -bool true
defaults write com.apple.Safari "com.apple.Safari.ContentPageGroupIdentifier.WebKit2DeveloperExtrasEnabled" -bool true
defaults write NSGlobalDomain WebKitDeveloperExtras -bool true

# Disable Webkit start page
defaults write org.webkit.nightly.WebKit StartPageDisabled -bool true

# Set Safari's home page to 'about:blank' for faster loading
defaults write com.apple.Safari HomePage -string "about:blank"

# Prevent Safari from opening 'safe' files automatically after downloading
defaults write com.apple.Safari AutoOpenSafeDownloads -bool false

# Hide Safari’s sidebar in Top Sites
defaults write com.apple.Safari ShowSidebarInTopSites -bool false

# Auto clear downloads
defaults write com.apple.Safari DownloadsClearingPolicy -int 2

# Don't fill passwords
defaults write com.apple.Safari AutoFillPasswords -bool false
defaults write com.apple.Safari AutoFillCreditCardData -int 0

# Show full URL in Safari
defaults write com.apple.Safari ShowFullURLInSmartSearchField -bool true

# Show status bar
defaults write com.apple.Safari ShowStatusBar -bool true
defaults write com.apple.Safari ShowStatusBarInFullScreen -bool true

# Toolbar setup
/usr/libexec/PlistBuddy -c "Delete :NSToolbar\\ Configuration\\ BrowserToolbarIdentifier-v2:TB\\ Item\\ Identifiers" ~/Library/Preferences/com.apple.Safari.plist &>/dev/null
/usr/libexec/PlistBuddy -c "Add :NSToolbar\\ Configuration\\ BrowserToolbarIdentifier-v2:TB\\ Item\\ Identifiers array" ~/Library/Preferences/com.apple.Safari.plist
items=(BackForwardToolbarIdentifier NSToolbarFlexibleSpaceItem InputFieldsToolbarIdentifier NSToolbarFlexibleSpaceItem ShareToolbarIdentifier)

for i in "${!items[@]}"; do
  /usr/libexec/PlistBuddy -c "Add :NSToolbar\\ Configuration\\ BrowserToolbarIdentifier-v2:TB\\ Item\\ Identifiers:$i string ${items[$i]}" ~/Library/Preferences/com.apple.Safari.plist
done

#
# Mail
#

# Only take address@example.com when copying email addresses in main
defaults write com.apple.mail AddressesIncludeNameOnPasteboard -bool false


#
# Messages
#

# Set to available when you come back from idle
defaults write com.apple.iChat WelcomeBackMode -int 1

# Hide scrollbars in Messages.app
defaults write com.apple.iChat AppleShowScrollBars -string Automatic

# Disable automatic emoji substitution (i.e. use plain text smileys)
defaults write com.apple.messageshelper.MessageController SOInputLineSettings -dict-add automaticEmojiSubstitutionEnablediMessage -bool false

# Disable smart quotes as it’s annoying for messages that contain code
defaults write com.apple.messageshelper.MessageController SOInputLineSettings -dict-add automaticQuoteSubstitutionEnabled -bool false

# Disable continuous spell checking
defaults write com.apple.messageshelper.MessageController SOInputLineSettings -dict-add continuousSpellCheckingEnabled -bool false


#
# iCal
#

defaults write com.apple.iCal "number of hours displayed" -int 16


#
# Other Applications
#

# Automatically quit printer app once the print jobs complete
defaults write com.apple.print.PrintingPrefs "Quit When Finished" -bool true

# Disable new disks for time machine warning
defaults write com.apple.TimeMachine DoNotOfferNewDisksForBackup -bool true

# Don’t display the annoying prompt when quitting iTerm
defaults write com.googlecode.iterm2 PromptOnQuit -bool false

# Sort contacts by first name
defaults write com.apple.AddressBook ABNameSortingFormat -string "sortingFirstName sortingLastName"

# Disk image changes
defaults write com.apple.frameworks.diskimages skip-verify -bool true
defaults write com.apple.frameworks.diskimages skip-verify-locked -bool true
defaults write com.apple.frameworks.diskimages skip-verify-remote -bool true
defaults write com.apple.DiskUtility DUDebugMenuEnabled 1
defaults write com.apple.DiskUtility advanced-image-options -bool true


#
# Other Interface changes
#

# Set Help Viewer windows to non-floating mode
defaults write com.apple.helpviewer DevMode -bool true

# Expand save panel by default
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true

# Expand print panel by default
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint -bool true


#
# Other subtle changes
#

# Save screenshots to the desktop
defaults write com.apple.screencapture location -string "$HOME/Desktop"

# Save screenshots in PNG format (other options: BMP, GIF, JPG, PDF, TIFF)
defaults write com.apple.screencapture type -string "png"

# Finally disable opening random Apple photo applications when plugging in devices
# https://twitter.com/stroughtonsmith/status/651854070496534528
defaults -currentHost write com.apple.ImageCapture disableHotPlug -bool true

# Check for software updates daily, not just once per week
defaults write com.apple.SoftwareUpdate ScheduleFrequency -int 1

# Enable automatic update & download
sudo defaults write /Library/Preferences/com.apple.SoftwareUpdate AutomaticCheckEnabled -bool true
sudo defaults write /Library/Preferences/com.apple.SoftwareUpdate AutomaticDownload -bool true

# Avoid creating .DS_Store files on network volumes
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true

# Shows ethernet connected computers in airdrop
defaults write com.apple.NetworkBrowser BrowseAllInterfaces -bool true


#
# X11
#

# Clipboard syncing
defaults write org.macosforge.xquartz.X11 sync_clipboard_to_pasteboard -bool true
defaults write org.macosforge.xquartz.X11 sync_pasteboard -bool true
defaults write org.macosforge.xquartz.X11 sync_pasteboard_to_clipboard -bool true
defaults write org.macosforge.xquartz.X11 sync_pasteboard_to_primary -bool true
defaults write org.macosforge.xquartz.X11 sync_primary_on_select -bool false

# Run xterm by default (without this vim's clipboard doesn't work)
defaults write org.macosforge.xquartz.X11 app_to_run -string /opt/X11/bin/xterm

# Set default shell
defaults write org.macosforge.xquartz.X11 login_shell -string /usr/local/bin/zsh


#
# Xcode
#

# Trim trailing whitespace
defaults write com.apple.dt.Xcode DVTTextEditorTrimTrailingWhitespace -bool true

# Trim whitespace only lines
defaults write com.apple.dt.Xcode DVTTextEditorTrimWhitespaceOnlyLines -bool true

# Don't show completion on escape
defaults write com.apple.dt.Xcode DVTTextShowCompletionsOnEsc -bool false

# Show line numbers
defaults write com.apple.dt.Xcode DVTTextShowLineNumbers -bool true

# Hide the code folding ribbon
defaults write com.apple.dt.Xcode DVTTextShowFoldingSidebar -bool false

# Enable automatic updates
defaults write com.apple.dt.Xcode DVTDownloadableAutomaticUpdate -bool true

# Live issues
defaults write com.apple.dt.Xcode IDEEnableLiveIssues -bool true

# Setup page guide
defaults write com.apple.dt.Xcode DVTTextShowPageGuide -bool true
defaults write com.apple.dt.Xcode DVTTextPageGuideLocation -int 110

# Max number of lines
defaults write com.apple.dt.Xcode IDEIssueNavigatorDetailLevel -int 10
defaults write com.apple.dt.Xcode IDESearchNavigatorDetailLevel -int 10

# Enable internal debug menu
defaults write com.apple.dt.Xcode ShowDVTDebugMenu -bool true

# Source control local revision side
defaults write com.apple.dt.Xcode DVTComparisonOrientationDefaultsKey -int 0

# Stack assitant editors vertically
defaults write com.apple.dt.Xcode AssistantEditorsLayout -int 1

# Use open quickly to open in the focused pane
defaults write com.apple.dt.Xcode IDEEditorCoordinatorTarget_Click -string FocusedEditor

# Use custom derived data location
defaults write com.apple.dt.Xcode IDECustomDerivedDataLocation -string build.noindex

# Show build times in toolbar
# http://cocoa.tumblr.com/post/131023038113/build-speed
defaults write com.apple.dt.Xcode ShowBuildOperationDuration -bool true

# Add more information to Xcode's build output about why specific commands are being run
# https://twitter.com/bdash/status/661742266487205888
# http://www.openradar.me/27516128
defaults write com.apple.dt.Xcode ExplainWhyBuildCommandsAreRun -bool true

# Stop Xcode from reopening files (specifically storyboards) on launch.
# I also have a `xcuser` shell command to wipe this state so you don't reopen
# storyboards and dirty the diff, or just take 30 seconds to launch
defaults write com.apple.dt.Xcode IDEDisableStateRestoration -bool true

# Write detailed build system info into derived data
# If you don't enable this but `mkdir /tmp/xcode_dependency_logs` the logs will
# be created there instead
defaults write com.apple.dt.Xcode EnableBuildSystemLogging -bool true

# Disable the print keyboard shortcut in Xcode. I accidentally hit this a lot
defaults write com.apple.dt.Xcode NSUserKeyEquivalents -dict-add "Print..." "nil"

# Enable extra logging for XCBuild
defaults write com.apple.dt.XCBuild EnableDebugActivityLogs -bool YES

# Make the tab key actually be a tab key
defaults write com.apple.dt.Xcode DVTTextTabKeyIndentBehavior -string Never

# Set the keybindings to my customizations (see $DOTFILES/xcode)
# Stored in ~/Library/Developer/Xcode/UserData/KeyBindings
defaults write com.apple.dt.Xcode IDEKeyBindingCurrentPreferenceSet -string custom.idekeybindings

# Set custom colorscheme
defaults write com.apple.dt.Xcode XCFontAndColorCurrentTheme -string panic.xccolortheme

# Show indexing progress
# https://twitter.com/dmartincy/status/1034930612543676418
defaults write com.apple.dt.Xcode IDEIndexerActivityShowNumericProgress -bool true

# Make command click jump to definition instead of showing the menu
defaults write com.apple.dt.Xcode IDECommandClickNavigates -bool YES


#
# Third Party
#

# Screeny don't record audio
defaults write com.drewwilson.screeny RecordAudio -bool false

# Tweetbot
# Skip t.co URLs
defaults write com.tapbots.TweetbotMac OpenURLsDirectly -bool true
defaults write com.tapbots.TweetbotMac openURLInBackground -bool true
defaults write com.tapbots.TweetbotMac showStatusItem -bool false
defaults write com.tapbots.TweetbotMac soundType -int 1
defaults write com.tapbots.TweetbotMac statusViewImageType -int 2
defaults write com.tapbots.TweetbotMac globalHideShowHotKey -dict \
  chars -string "c" \
  keyCode -int 8 \
  modifierFlags -int 1310985
# Setup a column on the right hand side with mentions
defaults delete com.tapbots.TweetbotMac MainWindowColumnInfo || true
defaults write com.tapbots.TweetbotMac MainWindowColumnInfo -array-add '{ "accountTID" = 14429563; "controllerClass" = "PTHTweetbotMentionsViewController"; "displayingMediaTimeline" = 0; }'

# Seil
defaults write org.pqrs.Seil sysctl -dict \
  enable_capslock -bool true \
  keycode_capslock -int 53

# Caffeine
defaults write com.lightheadsw.caffeine SuppressLaunchMessage -bool true

# ClipMenu
defaults write com.naotaka.ClipMenu maxHistorySize -int 100
defaults write com.naotaka.ClipMenu numberOfItemsPlaceInline -int 10
defaults write com.naotaka.ClipMenu showAlertBeforeClearHistory -bool false
defaults write com.naotaka.ClipMenu showStatusItem -bool false

# Firefox
defaults write org.mozilla.firefox AppleShowScrollBars -string "Automatic"

echo "Done. Note that some of these changes require a logout/restart to take effect."
# vim:tw=0
