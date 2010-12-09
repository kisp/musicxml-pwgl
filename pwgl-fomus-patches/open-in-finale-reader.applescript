tell application "Finale Reader" to activate

tell application "System Events"
	tell process "Finale Reader"
		click the menu item "Import MusicXMLÉ" of the menu "File" of menu bar 1
		keystroke "e.xml"
		keystroke return
	end tell
end tell
