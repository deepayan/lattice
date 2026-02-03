
svgin <- readLines("logo-template.svg.in")


export_svg <- function(outfile = "") {
    strsub <- function(x, from, to) gsub(from, to, x, fixed = TRUE)
    svgout <-
        (svgin |> 
            strsub("COLORONE", COLORONE) |> 
            strsub("COLORTWO", COLORTWO) |> 
            strsub("COLORTHR", COLORTHR) |> 
            strsub("COLORFOR", COLORFOR) |> 
            strsub("COLORBOR", COLORBOR))
    cat(svgout, file = outfile, sep = "\n")
}

COLORONE="6cb4d9"
COLORTWO="aca4e2"
COLORTHR="5cbd92"
COLORFOR="38bdbb"
COLORBOR="386cb0"

export_svg("logo.svg")


