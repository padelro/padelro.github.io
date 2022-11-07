namespace Common

open System
open System.Threading
open System.Windows.Forms
open System.Drawing
open System.Drawing.Drawing2D
open System.Drawing.Text
open System.Diagnostics
open System.Reflection
open System.Runtime.Versioning
open System.Runtime.InteropServices

module UI =

    type DxForm(size: int * int) as __ =
        inherit Form(Text = "Main")
        let (w, h) = size

        do
            __.SetStyle(ControlStyles.AllPaintingInWmPaint ||| ControlStyles.UserPaint ||| ControlStyles.ResizeRedraw, true)
            __.ResizeRedraw <- true
            __.StartPosition <- FormStartPosition.Manual
            __.Location <- Point(50, 100)
            __.ClientSize <- Size(w, h)

        override __.OnLoad(evt) =
            do base.OnLoad(evt)

            __.KeyDown.Add(fun key ->
                match key.KeyCode with
                | Keys.Escape -> __.Close()
                | _  -> ()
            )