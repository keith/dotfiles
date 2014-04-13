#!/usr/bin/env bash

# ~/.osx — http://mths.be/osx

if [[ $# -ne 1 ]];then
    echo "Usage: ./$(basename $0) COMPNAME"
    exit
fi

killall System\ Preferences

# Ask for the administrator password upfront
sudo -v

# Keep-alive: update existing 'sudo' time stamp until '.osx' has finished
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

# Set computer name
name=$1
sudo scutil --set ComputerName $name
sudo scutil --set HostName $name
sudo scutil --set LocalHostName $name
sudo defaults write /Library/Preferences/SystemConfiguration/com.apple.smb.server NetBIOSName -string $name


#
# General Settings
#

# Default sidebar icon size to small
defaults write NSGlobalDomain NSTableViewDefaultSizeMode -int 1

# Ask to keep changes on close
defaults write NSGlobalDomain NSCloseAlwaysConfirmsChanges -int 1

# Disables shutting down inactive applications
defaults write NSGlobalDomain NSDisableAutomaticTermination -bool true

# Disable the “reopen windows when logging back in” option
# This works, although the checkbox will still appear to be checked.
defaults write com.apple.loginwindow TALLogoutSavesState -bool false
defaults write com.apple.loginwindow LoginwindowLaunchesRelaunchApps -bool false

# Disabling App Nap
#defaults write NSGlobalDomain NSAppSleepDisabled -bool YES

# Always show scrollbars
defaults write NSGlobalDomain AppleShowScrollBars -string "Always"

# Disable Resume system-wide
defaults write NSGlobalDomain NSQuitAlwaysKeepsWindows -int 0

# Enable subpixel font rendering on non-Apple LCDs
defaults write NSGlobalDomain AppleFontSmoothing -int 2


#
# Desktop & Screen Saver
#

# Disable menu bar transparency
defaults write NSGlobalDomain AppleEnableMenuBarTransparency -bool false

# Disable the screen saver (System Preferences must be closed)
defaults -currentHost write com.apple.screensaver idleTime -int 0

# Ask for password for screensaver
defaults write com.apple.screensaver askForPassword -int 1

# Set delay before password ask (minutes x 60)
defaults write com.apple.screensaver askForPasswordDelay -int 900

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
defaults write com.apple.dock autohide-delay -float 0.05
defaults write com.apple.dock autohide-time-modifier -float 0.05

# Show indicator lights for open applications in the Dock
defaults write com.apple.dock show-process-indicators -bool true

# 2D Dock
defaults write com.apple.dock no-glass -bool true

# Make Dock icons of hidden applications translucent
defaults write com.apple.dock showhidden -bool true

# Dock tweaks
defaults write com.apple.dock mouse-over-hilte-stack -bool true


#
# Mission Control
#

# Don't show Dashboard as a Space
defaults write com.apple.dock dashboard-in-overlay -bool true

# Don't automatically rearrange Spaces based on most recent use
defaults write com.apple.dock mru-spaces -bool false

# Run hot corners script
if [[ -f ../scripts/corners.sh ]]; then
    ../scripts/corners.sh enable
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
defaults write com.apple.airplay showInMenuBarIfPresent -bool true

# Enable HiDPI display modes (requires restart)
sudo defaults write /Library/Preferences/com.apple.windowserver DisplayResolutionEnabled -bool true


#
# Power Settings
#

# To stop the display from half dimming before full display 'sleep' [docs](http://developer.apple.com/library/mac/#documentation/Darwin/Reference/ManPages/man1/pmset.1.html)
sudo pmset -a halfdim 0

# Sleep options
sudo pmset -a displaysleep 5
sudo pmset -a disksleep 0
sudo pmset -a sleep 0

# Wake for network access
sudo pmset -a womp 1

# Power button shows shutdown dialog <=10.8
# Removed *intentionally* in Mavericks
# sudo pmset -a powerbutton 0

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

# Disable Sleep image
sudo pmset hibernatemode 0
sudo rm -rf /var/vm/sleepimage

# Power button behavior
defaults write com.apple.loginwindow PowerButtonSleepsSystem -bool NO


#
# Keyboard
#

# Enable full keyboard access for all controls (e.g. enable Tab in modal dialogs)
defaults write NSGlobalDomain AppleKeyboardUIMode -int 3

# Set a blazingly fast keyboard repeat rate
defaults write NSGlobalDomain KeyRepeat -int 0


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


#
# Sound
#

# Change the error sound
defaults write com.apple.systemsound com.apple.sound.beep.sound -string "/System/Library/Sounds/Morse.aiff"

# Disable volume change feedback
defaults write NSGlobalDomain com.apple.sound.beep.feedback -int 0


#
# Date/Time
#

# Setup the menu bar date format
defaults write com.apple.menuextra.clock DateFormat -string "EEE MMM d  h:mm a"

# Flash the : in the menu bar
defaults write com.apple.menuextra.clock FlashDateSeparators -int 1


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
defaults write com.apple.finder FXPreferredViewStyle -string "clmv"

# Allow text selection in QuickLook
defaults write com.apple.finder QLEnableTextSelection -bool true

# Display full POSIX path as Finder window title
defaults write com.apple.finder _FXShowPosixPathInTitle -bool true

# When performing a search, search the current folder by default
defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"

# Disable the warning when changing a file extension
defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false

# Default to local files instead of iCloud
defaults write NSGlobalDomain NSDocumentSaveNewDocumentsToCloud -bool false

# Disable the warning before emptying the Trash
defaults write com.apple.finder WarnOnEmptyTrash -bool false

# Should remove downloaded from the internet warnings
defaults write com.apple.LaunchServices LSQuarantine -bool false

# Enable highlight hover effect for the grid view of a stack (Dock)
defaults write com.apple.dock mouse-over-hilite-stack -bool true

# Show the ~/Library folder
chflags nohidden $HOME/Library

# Automatically open a new Finder window when a volume is mounted
defaults write com.apple.frameworks.diskimages auto-open-ro-root -bool true
defaults write com.apple.frameworks.diskimages auto-open-rw-root -bool true
defaults write com.apple.finder OpenWindowForNewRemovableDisk -bool true

# Enable snap-to-grid for icons on the desktop and in other icon views
/usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:arrangeBy grid" ~/Library/Preferences/com.apple.finder.plist
/usr/libexec/PlistBuddy -c "Set :FK_StandardViewSettings:IconViewSettings:arrangeBy grid" ~/Library/Preferences/com.apple.finder.plist
/usr/libexec/PlistBuddy -c "Set :StandardViewSettings:IconViewSettings:arrangeBy grid" ~/Library/Preferences/com.apple.finder.plist

# Show item info near icons on the desktop and in other icon views
/usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:showItemInfo true" ~/Library/Preferences/com.apple.finder.plist
/usr/libexec/PlistBuddy -c "Set :FK_StandardViewSettings:IconViewSettings:showItemInfo true" ~/Library/Preferences/com.apple.finder.plist
/usr/libexec/PlistBuddy -c "Set :StandardViewSettings:IconViewSettings:showItemInfo true" ~/Library/Preferences/com.apple.finder.plist

# Increase grid spacing for icons on the desktop and in other icon views
/usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:gridSpacing 64" ~/Library/Preferences/com.apple.finder.plist
/usr/libexec/PlistBuddy -c "Set :FK_StandardViewSettings:IconViewSettings:gridSpacing 64" ~/Library/Preferences/com.apple.finder.plist
/usr/libexec/PlistBuddy -c "Set :StandardViewSettings:IconViewSettings:gridSpacing 64" ~/Library/Preferences/com.apple.finder.plist

# Increase the size of icons on the desktop and in other icon views
/usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:iconSize 64" ~/Library/Preferences/com.apple.finder.plist
/usr/libexec/PlistBuddy -c "Set :FK_StandardViewSettings:IconViewSettings:iconSize 64" ~/Library/Preferences/com.apple.finder.plist
/usr/libexec/PlistBuddy -c "Set :StandardViewSettings:IconViewSettings:iconSize 64" ~/Library/Preferences/com.apple.finder.plist


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
defaults write org.webkit.nightly.WebKit StartPageDisabled -bool YES

# Set Safari's home page to 'about:blank' for faster loading
defaults write com.apple.Safari HomePage -string "about:blank"

# Prevent Safari from opening 'safe' files automatically after downloading
defaults write com.apple.Safari AutoOpenSafeDownloads -bool false


#
# Mail
#

# Only take address@example.com when copying email addresses in main
defaults write com.apple.mail AddressesIncludeNameOnPasteboard -bool false


#
# iChat
#

# Setup iChat's global hotkey
defaults write com.apple.iChat GlobalKeyActive -int 1
defaults write com.apple.iChat GlobalKeyCode -int 36
defaults write com.apple.iChat GlobalKeyModifiers -int 768


#
# Other Applications
#

# Automatically quit printer app once the print jobs complete
defaults write com.apple.print.PrintingPrefs "Quit When Finished" -bool true

# Disable new disks for time machine warning
defaults write com.apple.TimeMachine DoNotOfferNewDisksForBackup -bool true

# Don’t display the annoying prompt when quitting iTerm
defaults write com.googlecode.iterm2 PromptOnQuit -bool false

# Disk image changes
defaults write com.apple.frameworks.diskimages skip-verify -bool true
defaults write com.apple.frameworks.diskimages skip-verify-locked -bool true
defaults write com.apple.frameworks.diskimages skip-verify-remote -bool true
defaults write com.apple.DiskUtility DUDebugMenuEnabled 1


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

# Disable shadow in screenshots
# defaults write com.apple.screencapture disable-shadow -bool true

# Check for software updates daily, not just once per week
defaults write com.apple.SoftwareUpdate ScheduleFrequency -int 1

# Avoid creating .DS_Store files on network volumes
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true

# Shows ethernet connected computers in airdrop
defaults write com.apple.NetworkBrowser BrowseAllInterfaces -bool true


#
# X11
#

# Clipboard syncing
defaults write org.macosforge.xquartz.X11 sync_clipboard_to_pasteboard -boolean true
defaults write org.macosforge.xquartz.X11 sync_pasteboard -boolean true
defaults write org.macosforge.xquartz.X11 sync_pasteboard_to_clipboard -boolean true
defaults write org.macosforge.xquartz.X11 sync_pasteboard_to_primary -boolean true
defaults write org.macosforge.xquartz.X11 sync_primary_on_select -boolean false

# Run xterm by default (without this vim's clipboard doesn't work)
defaults write org.macosforge.xquartz.X11 app_to_run -string "/opt/X11/bin/xterm"

# Set default shell
defaults write org.macosforge.xquartz.X11 login_shell -string "/usr/local/bin/zsh"


# Killing affected applications
for app in Safari Finder Dock iTerm2 Mail Messages SystemUIServer Xquartz
do
    killall "$app" >/dev/null 2>&1
done

# Done. Note that some of these changes require a logout/restart to take effect.
