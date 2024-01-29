try
    tell application "DrRacket" to activate
    tell application "System Events"
        keystroke "3"
    end tell
on error errMsg
    display dialog errMsg
end try