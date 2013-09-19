set h to hours of (current date)
if (h > 7) and (h < 19) then
	set tint to "light"
else
	set tint to "dark"
end if

tell application "iTerm"
	activate
	set t to current terminal
	try
		get t
	on error
		set t to (make new terminal)
	end try
	
	repeat with t in terminals
		tell t
			repeat with s in sessions
				try
					get s
				on error
					set s to launch session "Default Session"
				end try
				
				tell s
					if tint is "light" then
						set ansiWhiteColor to {6.003730859375E+4, 5.83269609375E+4, 5.2284546875E+4}
						set selected text color to {1.813483984375E+4, 2.337368359375E+4, 2.50989140625E+4}
						set cursor_text color to {6.003730859375E+4, 5.83269609375E+4, 5.2284546875E+4}
						set ansiBrightWhiteColor to {6.48425703125E+4, 6.277885546875E+4, 5.662616015625E+4}
						set ansiGreenColor to {2.9475974609375E+4, 3.54645859375E+4, 1324.3807373047}
						set ansiBrightBlackColor to {0.0, 7722.3891601562, 9941.8388671875}
						set ansiBrightYellowColor to {2.1257337890625E+4, 2.6684328125E+4, 2.8737466796875E+4}
						set bold color to {1.813483984375E+4, 2.337368359375E+4, 2.50989140625E+4}
						set ansiYellowColor to {4.24316015625E+4, 3.0638546875E+4, 1539.0771484375}
						set ansiBrightRedColor to {4.861141015625E+4, 1.39755361328125E+4, 4818.8168945312}
						set ansiBrightCyanColor to {3.31601796875E+4, 3.70179921875E+4, 3.6937921875E+4}
						set background color to {6.48425703125E+4, 6.277885546875E+4, 5.662616015625E+4}
						set ansiBrightGreenColor to {1.813483984375E+4, 2.337368359375E+4, 2.50989140625E+4}
						set ansiBlueColor to {8358.9130859375, 3.0320388671875E+4, 5.12689609375E+4}
						set cursor color to {2.1257337890625E+4, 2.6684328125E+4, 2.8737466796875E+4}
						set ansiBrightMagentaColor to {2.280528515625E+4, 2.22139375E+4, 4.778054296875E+4}
						set ansiMagentaColor to {5.094621484375E+4, 7079.39453125, 2.8518626953125E+4}
						set AnsiBlackColor to {0.0, 1.020768359375E+4, 1.2694220703125E+4}
						set ansiCyanColor to {9620.2333984375, 3.740892578125E+4, 3.44073671875E+4}
						set selection color to {6.003730859375E+4, 5.83269609375E+4, 5.2284546875E+4}
						set ansiBrightBlueColor to {2.887342578125E+4, 3.339855859375E+4, 3.38722890625E+4}
						set foreground color to {2.1257337890625E+4, 2.6684328125E+4, 2.8737466796875E+4}
						set ansiRedColor to {5.369084765625E+4, 7104.4169921875, 9270.392578125}
					else
						set ansiWhiteColor to {6.003730859375E+4, 5.83269609375E+4, 5.2284546875E+4}
						set selected text color to {3.31601796875E+4, 3.70179921875E+4, 3.6937921875E+4}
						set cursor_text color to {0.0, 1.020768359375E+4, 1.2694220703125E+4}
						set ansiBrightWhiteColor to {6.48425703125E+4, 6.277885546875E+4, 5.662616015625E+4}
						set ansiGreenColor to {2.9475974609375E+4, 3.54645859375E+4, 1324.3807373047}
						set ansiBrightBlackColor to {0.0, 7722.3891601562, 9941.8388671875}
						set ansiBrightYellowColor to {2.1257337890625E+4, 2.6684328125E+4, 2.8737466796875E+4}
						set bold color to {3.31601796875E+4, 3.70179921875E+4, 3.6937921875E+4}
						set ansiYellowColor to {4.24316015625E+4, 3.0638546875E+4, 1539.0771484375}
						set ansiBrightRedColor to {4.861141015625E+4, 1.39755361328125E+4, 4818.8168945312}
						set ansiBrightCyanColor to {3.31601796875E+4, 3.70179921875E+4, 3.6937921875E+4}
						set background color to {0.0, 7722.3891601562, 9941.8388671875}
						set ansiBrightGreenColor to {1.813483984375E+4, 2.337368359375E+4, 2.50989140625E+4}
						set ansiBlueColor to {8358.9130859375, 3.0320388671875E+4, 5.12689609375E+4}
						set cursor color to {2.887342578125E+4, 3.339855859375E+4, 3.38722890625E+4}
						set ansiBrightMagentaColor to {2.280528515625E+4, 2.22139375E+4, 4.778054296875E+4}
						set ansiMagentaColor to {5.094621484375E+4, 7079.39453125, 2.8518626953125E+4}
						set AnsiBlackColor to {0.0, 1.020768359375E+4, 1.2694220703125E+4}
						set ansiCyanColor to {9620.2333984375, 3.740892578125E+4, 3.44073671875E+4}
						set selection color to {0.0, 1.020768359375E+4, 1.2694220703125E+4}
						set ansiBrightBlueColor to {2.887342578125E+4, 3.339855859375E+4, 3.38722890625E+4}
						set foreground color to {2.887342578125E+4, 3.339855859375E+4, 3.38722890625E+4}
						set ansiRedColor to {5.369084375E+4, 7104.4233398438, 9270.3935546875}
					end if
				end tell
			end repeat
		end tell
	end repeat
end tell

return
