-- Imports
require 'cairo'

local home = os.getenv("HOME")

-- --------------------------------------- Variables ----------------------------------------

-- Variables
local color1 = { ["r"] = 0.36, ["g"] = 0.63, ["b"] = 0.06 } 	-- >90 - 80%
local color2 = { ["r"] = 0.49, ["g"] = 0.75, ["b"] = 0.20 }
local color3 = { ["r"] = 0.67, ["g"] = 0.86, ["b"] = 0.15 }
local color4 = { ["r"] = 0.96, ["g"] = 0.66, ["b"] = 0 }
local color5 = { ["r"] = 0.96, ["g"] = 0.45, ["b"] = 0 }
local color6 = { ["r"] = 0.85, ["g"] = 0.18, ["b"] = 0 }	-- >10%

-- 1-100, 0 to disable the feature
local alarm = 10

-- Move inside the Conky window, alignment top_left
local gap_x = 0
local gap_y = 0

-- ------------------------------------ End of variables -------------------------------------

function conky_drawBattery()

	-- Check Conky windows status
	if conky_window == nil then return end
		
	-- Set-up cairo
	local cs = cairo_xlib_surface_create(conky_window.display, conky_window.drawable, conky_window.visual, conky_window.width, conky_window.height)
	cr = cairo_create(cs)

-- --------------------------- Write your code your below this line ------------------------

	local battery_percent = tonumber(conky_parse('${battery_percent}'))
	local battery_short = tostring(conky_parse('${battery_short}'))

	-- Set bar color
	if     (battery_percent >90) 	then	cairo_set_source_rgba(cr, color1.r, color1.g, color1.b, 1.0)
	elseif (battery_percent >70)	then	cairo_set_source_rgba(cr, color2.r, color2.g, color2.b, 1.0)
    	elseif (battery_percent >50)	then	cairo_set_source_rgba(cr, color3.r, color3.g, color3.b, 1.0)
    	elseif (battery_percent >40)	then	cairo_set_source_rgba(cr, color4.r, color4.g, color4.b, 1.0)
    	elseif (battery_percent >20)	then	cairo_set_source_rgba(cr, color5.r, color5.g, color5.b, 1.0)
    	elseif (battery_percent >10)	then	cairo_set_source_rgba(cr, color6.r, color6.g, color6.b, 1.0) end	

	-- Make bar
	cairo_rectangle(cr,
			gap_x + 6,
			gap_y + 15,
			33 * battery_percent / 100,
			20
	)
	cairo_fill(cr)

	-- Set picture	
	local path = nil
	if (string.sub(battery_short, 1, 1) == "C") then
		-- set charging
		path = home .. '/.conky/TinyBattery/images/BatteryPlug.png'
	else
		if (battery_percent < alarm) then
			-- Alarm
			path = home .. '/.conky/TinyBattery/images/BatteryOut.png'
		else
			-- Normal
			path = home .. '/.conky/TinyBattery/images/Battery.png'
		end
	end

	local image = cairo_image_surface_create_from_png(path)
	cairo_set_source_surface(cr, image, 0, 0)
	cairo_paint(cr)

-- ----------------------------------- End of your code ------------------------------------
	
	-- Clean up Cairo
	cairo_destroy(cr)
	cairo_surface_destroy(cs)
	cr = nil		
end

