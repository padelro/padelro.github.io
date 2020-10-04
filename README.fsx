open System

#if INTERACTIVE
#load ".paket/load/main.group.fsx"
#endif

open Fable.React

let HOME_URL =  "https://padelro.github.io/"

let CSS_STYLES = """
body {
    margin: 0 auto;
    color: #454545;
    font-family: 'Iosevka SS08', sans-serif;
    font-size: 13px;
    line-height: 1.4;
    text-shadow: 0 1px 0 #ffffff;
    max-width: 63%;
    background-image: linear-gradient(0deg,rgb(255, 255, 255), rgba(255, 255, 255, 0.80)), url('./adata/Help-Me.jpg');
    background-position: center center;
    background-repeat: no-repeat;
    background-size: cover;
    min-height: 100vh;
}

code {
    background: white;
    font-family: 'Iosevka SS08', monospace;
}

a {
    font-weight: 400;
    border-bottom: 1px dotted #0d80bb;
    color: #030303;
    text-decoration: none;
    padding: 2px 5px;
    display: inline-block;
    background-image: linear-gradient(transparent 12px, #e1ff00 0);
}

a:hover {
    font-weight: 600;
    color: #0d80bb;
    border-bottom: 1px solid #030303;
    background-image: linear-gradient(transparent 18px, #e4e4e4 0);
}

blockquote {
    font-style: italic;
    color: #9a1a1a;
}

section#main {
    display: flex;
}

footer {
    margin-top: 10px;
}

h1, h2 {
    font-weight: 600;
}

nav ul {
    list-style-type: none;
}

nav ul li {
    display: inline-block;
    margin-top: 0.4em;
}

.hidden {
    display: none;
}

#main-list-of-stuff div {
    padding: 1.4em 0;
    border-bottom: solid 1px #e3e3e3;
}
"""

let CSS_LOGO = """
.std_bg         { stroke: transparent; stroke-width:   1; fill: transparent }
.std_solid_line { stroke:       white; stroke-width:  6%; fill: white }
.std_mask_line  { stroke:         red; stroke-width:  6%; fill: black }

.std_mask       { stroke: transparent; stroke-width:   1; fill: black }
.std_solid      { stroke:       white; stroke-width:   1; fill: white }
"""

[ 5 .. -1 .. 0 ]
|> List.map (printfn "%i..")
|> ignore

let template htmlBody =
    let logo =
        svg [ Props.Width "250"; Props.Height "250"; Props.ViewBox "0,0 100%,100%" ] [
            defs [] [
                clipPath [ Props.Id "svgPath" ] [
                    rect [ Props.X "15%"; Props.Y "15%"; Props.Width "70%"; Props.Height "70%" ] []
                ]
            ]

            rect   [ Props.Class   "std_bg"; Props.X   "0"; Props.Y   "0"; Props.Width "100%"; Props.Height "100%" ] []
            rect   [ Props.Class "std_mask"; Props.X "25%"; Props.Y "25%"; Props.Width  "50%"; Props.Height  "50%" ] []

            circle [ Props.Class "std_solid"; Props.R "16%"; Props.Cx "50%"; Props.Cy "50%" ] []
            circle [ Props.Class  "std_mask"; Props.R "10%"; Props.Cx "50%"; Props.Cy "50%" ] []

            g [ Props.Class "std_mask_line" ] [
                line [ Props.Class "std_solid_line"; Props.X1 "37%"; Props.Y1 "50%"; Props.X2 "37%"; Props.Y2 "95%" ] []
                line [ Props.Class "std_solid_line"; Props.X1 "63%"; Props.Y1  "5%"; Props.X2 "63%"; Props.Y2 "50%" ] []

                line [ Props.Class  "std_mask_line"; Props.X1 "10%"; Props.Y1 "10%"; Props.X2 "90%"; Props.Y2 "90%"; Props.Id "target"; Props.ClipPath "url(#svgPath)" ] []
            ]
        ]

    html [] [
        head [] [
            meta [ Props.Name "viewport"; Props.Content "width=device-width, height=device-height, user-scalable=no, initial-scale=1.0" ]
            meta [ Props.Name "charset"; Props.Content "utf-8" ]
            link [ Props.Rel "shortcut icon"; Props.Href "favicon.ico" ]

            title [] [ str "Padel" ]

            style [] [ RawText CSS_STYLES ]
            style [] [ RawText CSS_LOGO ]
        ]

        header [] [
            logo
            section [ Props.Id "main" ] [
                h1 [ Props.Id "title" ] [ str "🕳️" ]
                nav [] [
                    ul [] [
                        li [] [ a [ Props.Href HOME_URL ] [ str "TODO" ] ]
                    ]
                ]
            ]
        ]

        body [] [
            main [] htmlBody
        ]

        footer [] [
            small [] [
                RawText "&copy;"
                str <| sprintf "%i META" DateTime.Now.Year
                RawText "&nbsp;|&nbsp;"
                a [ Props.Href HOME_URL ] [ str "padelro.github.io" ]
            ]
        ]
    ]

let ``Hello world`` =
    div [] [
        h2 [] [ str "Hello World"; RawText "&nbsp;"; small [] [ str "📆 04 oct. 2020" ] ]
        p [] [ str "I'm hosted with GitHub Pages." ]
        small [] [ str "Under construction or something..." ]
    ]

let html =
    [
        section [ Props.Id "main-list-of-stuff" ] [
            ``Hello world``

            p [] [ str "..." ]
        ]

        section [ Props.Id "draft" ] [
            h2 [] [ str "🏖️" ]
            p [ Props.Class "hidden" ] [
                str "This is equivalent to HTML!"
                small [] [ str "(no need for anything else)" ]
            ]
        ]
    ]

do
    ("<!DOCTYPE html>\n" + (template html |> Fable.ReactServer.renderToString))
    |> Text.Encoding.UTF8.GetBytes
    |> fun bytes -> ( "index.html", bytes )
    |> IO.File.WriteAllBytes

    let ci = Globalization.CultureInfo.CreateSpecificCulture("ro-RO")
    DateTime.Now.ToString("dd MMM yyyy", ci)
    |> printfn "%s"
