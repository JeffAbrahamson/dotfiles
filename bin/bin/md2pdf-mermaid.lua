-- Render fenced Mermaid code blocks as vector images for md2pdf.

local mmdc_available
local mmdc_pdf_fit
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

    if mmdc_pdf_fit == nil then
        local help = pandoc.pipe("mmdc", {"--help"}, "")
        -- Mermaid CLI 12 fits PDFs by default and removed --pdfFit.
        mmdc_pdf_fit = help:find("--pdfFit", 1, true) ~= nil
    end

    pandoc.system.with_temporary_directory("md2pdf-mermaid", function(directory)
        local input = directory .. "/diagram.mmd"
        local pdf = directory .. "/diagram.pdf"

        write_file(input, source)
        local arguments = {"--input", input, "--output", pdf, "--quiet"}
        if mmdc_pdf_fit then
            table.insert(arguments, "--pdfFit")
        end
        pandoc.pipe("mmdc", arguments, "")
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
