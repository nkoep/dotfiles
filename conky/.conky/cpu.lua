require 'cairo'

local color, fg_alpha, bg_alpha = 0x101010, 1.0, 0.2
local xspacing, xpos, ypos = 30, 175, 117
local radius, thickness, start_angle, end_angle = 10, 3, 230, 490

function parse_rgb(color)
    return ((color / 0x10000) % 0x100) / 255.0,
           ((color / 0x100) % 0x100) / 255.0,
           (color % 0x100) / 255.0
end

function conky_cpu()
    local function draw_cpu_ring(cr, idx)
        local str = string.format('${cpu cpu%d}', idx)
        local usage = tonumber(conky_parse(str))
        if usage == nil then
            usage = 0
        end

        local xpos_ = xpos + idx * xspacing;

        local angle_0 = start_angle * (2*math.pi/360)-math.pi/2
        local angle_f = end_angle * (2*math.pi/360)-math.pi/2
        local angle = usage * (angle_f-angle_0) / 100

        -- Draw base ring.
        local r, g, b = parse_rgb(color)
        cairo_arc(cr, xpos_, ypos, radius, angle_0, angle_f)
        cairo_set_source_rgba(cr, r, g, b, bg_alpha)
        cairo_set_line_width(cr, thickness)
        cairo_stroke(cr)

        -- Draw indicator ring.
        cairo_arc(cr, xpos_, ypos, radius, angle_0, angle_0+angle)
        cairo_set_source_rgba(cr, r, g, b, fg_alpha)
        cairo_stroke(cr)
    end

    if conky_window == nil then
        return
    end

    local cs = cairo_xlib_surface_create(
        conky_window.display, conky_window.drawable, conky_window.visual,
        conky_window.width, conky_window.height)
    local cr = cairo_create(cs)

    if tonumber(conky_parse('${updates}')) > 5 then
        for i = 0, 3 do
            draw_cpu_ring(cr, i)
        end
    end

    cairo_destroy(cr)
    cairo_surface_destroy(cs)
end

