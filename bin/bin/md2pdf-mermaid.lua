-- Render fenced Mermaid code blocks as vector images for md2pdf.

local mmdc_available
local warned_about_mmdc = false

local function has_mmdc()
    if mmdc_available == nil then
        mmdc_available = pcall(
            pandoc.pipe,
            "sh",
            {"-c", "command -v mmdc"},
            ""
        )
    end
    return mmdc_available
end

local function has_class(block, wanted)
    for _, class in ipairs(block.classes) do
        if class == wanted then
            return true
        end
    end
    return false
end

local function read_file(path)
    local file, message = io.open(path, "rb")
    if not file then
        error("md2pdf: cannot read rendered Mermaid diagram: " .. message)
    end

    local contents = file:read("*a")
    file:close()
    return contents
end

local function write_file(path, contents)
    local file, message = io.open(path, "wb")
    if not file then
        error("md2pdf: cannot create temporary Mermaid input: " .. message)
    end

    file:write(contents)
    file:close()
end

local function render_mermaid(source)
    local rendered

    pandoc.system.with_temporary_directory("md2pdf-mermaid", function(directory)
        local input = directory .. "/diagram.mmd"
        local pdf = directory .. "/diagram.pdf"

        write_file(input, source)
        pandoc.pipe(
            "mmdc",
            {"--input", input, "--output", pdf, "--pdfFit", "--quiet"},
            ""
        )
        rendered = read_file(pdf)
    end)

    return rendered
end

function CodeBlock(block)
    if not has_class(block, "mermaid") then
        return nil
    end

    if not has_mmdc() then
        if not warned_about_mmdc then
            io.stderr:write(
                "md2pdf: warning: mmdc is not installed; "
                    .. "leaving Mermaid blocks as code\n"
            )
            warned_about_mmdc = true
        end
        return nil
    end

    local pdf = render_mermaid(block.text)
    local filename = "mermaid-" .. pandoc.sha1(pdf) .. ".pdf"
    pandoc.mediabag.insert(filename, "application/pdf", pdf)

    local description = block.attributes["title"] or "Mermaid diagram"
    local image = pandoc.Image({pandoc.Str(description)}, filename)
    image.attributes["width"] = "100%"
    image.attributes["height"] = "80%"
    return pandoc.Para({image})
end
