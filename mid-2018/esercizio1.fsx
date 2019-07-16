open System.Windows.Forms
open System.Drawing
open System.Collections.ObjectModel
open System

// ======================================================================================================== \\

type WVMatrix () =

    let wv = new Drawing2D.Matrix()
    let vw = new Drawing2D.Matrix()

    member this.TranslateWorld (tx,ty) =
        wv.Translate(tx,ty)
        vw.Translate(-tx,-ty,Drawing2D.MatrixOrder.Append)

    member this.ScaleWorld (sx,sy) =
        wv.Scale(sx,sy)
        vw.Scale(1.f / sx, 1.f / sy, Drawing2D.MatrixOrder.Append)

    member this.RotateWorld (a) =
        wv.Rotate(a)
        vw.Rotate(-a,Drawing2D.MatrixOrder.Append)

    member this.TranslateView (tx,ty) =
        vw.Translate(tx,ty)
        wv.Translate(-tx,-ty,Drawing2D.MatrixOrder.Append)

    member this.ScaleView (sx,sy) =
        vw.Scale(sx,sy)
        wv.Scale(1.f / sx, 1.f / sy, Drawing2D.MatrixOrder.Append)

    member this.RotateView (a) =
        vw.Rotate(a)
        wv.Rotate(-a,Drawing2D.MatrixOrder.Append)

    member this.TransformPointView (p:PointF) =
      let a = [| p |]
      vw.TransformPoints(a)
      a.[0]

    member this.TransformPointWorld (p:PointF) =
      let a = [| p |]
      wv.TransformPoints(a)
      a.[0]

    member this.VW with get() = vw
    member this.WV with get() = wv

// ======================================================================================================== \\

type LWCControl() = 

    let wv = WVMatrix()

    let mutable size = SizeF(70.f, 25.f)

    let mutable position = PointF()

    let mutable parent : LWCContainer option = None

    member this.WV = wv

    abstract OnPaint : PaintEventArgs -> unit
    default this.OnPaint(e) = ()

    abstract OnMouseDoubleClick : MouseEventArgs -> unit
    default this.OnMouseDoubleClick(e) = ()

    abstract OnMouseDown : MouseEventArgs -> unit
    default this.OnMouseDown(e) = ()

    abstract OnMouseUp : MouseEventArgs -> unit
    default this.OnMouseUp(e) = ()

    abstract OnMouseMove : MouseEventArgs -> unit
    default this.OnMouseMove(e) = ()

    abstract OnKeyDown : KeyEventArgs -> unit
    default this.OnKeyDown(e) =()

    abstract OnKeyUp : KeyEventArgs -> unit
    default this.OnKeyUp(e) =()

    member this.Parent 
        with get() = parent
        and set(v) = parent <- v

    member this.Size 
        with get() = size
        and set(v) = 
            size <- v
            this.Invalidate()

    member this.Position
        with get() = position
        and set(v) = 
            wv.TranslateView(position.X, position.Y)
            position <- v
            wv.TranslateView(-position.X, -position.Y)
            this.Invalidate()

    member this.PositionInt with get() = Point(int position.X, int position.Y)

    member this.SizeInt with get() = Size(int size.Width, int size.Height)

    member this.Left = position.X

    member this.Top = position.Y

    member this.Width = size.Width
    member this.Height = size.Height

    member this.HitTest(p:Point) =
        let pt = wv.TransformPointView(PointF(single p.X, single p.Y))
        let boundingBox = RectangleF(0.f, 0.f, size.Width, size.Height)
        boundingBox.Contains(pt)

    member this.Invalidate() = 
        match parent with 
        | Some p -> p.Invalidate()
        | None -> ()

and LWCContainer() as this =
    inherit UserControl()

    let controls = ObservableCollection<LWCControl>()

    do
        this.SetStyle(ControlStyles.OptimizedDoubleBuffer, true)
        controls.CollectionChanged.Add(fun e ->
            for i in e.NewItems do
                (i :?> LWCControl).Parent <- Some(this)
        )

    member this.LWControls with get() = controls

    override this.OnMouseDown(e) =
        let oc = controls |> Seq.tryFindBack(fun c -> c.HitTest(e.Location))
        match oc with 
        | Some c -> 
            let p = c.WV.TransformPointView(PointF(single e.X, single e.Y))
            let evt = new MouseEventArgs(e.Button, e.Clicks, int p.X, int p.Y, e.Delta)
            c.OnMouseDown(evt)
        | None -> ()

    override this.OnMouseUp(e) =
        let oc = controls |> Seq.tryFindBack(fun c -> c.HitTest(e.Location))
        match oc with 
        | Some c -> 
            let p = c.WV.TransformPointView(PointF(single e.X, single e.Y))
            let evt = new MouseEventArgs(e.Button, e.Clicks, int p.X, int p.Y, e.Delta)
            c.OnMouseUp(evt)
        | None -> ()

    override this.OnMouseMove(e) =
        let oc = controls |> Seq.tryFindBack(fun c -> c.HitTest(e.Location))
        match oc with 
        | Some c -> 
            let p = c.WV.TransformPointView(PointF(single e.X, single e.Y))
            let evt = new MouseEventArgs(e.Button, e.Clicks, int p.X, int p.Y, e.Delta)
            c.OnMouseMove(evt)
        | None -> ()

    override this.OnMouseDoubleClick(e) =
        let oc = controls |> Seq.tryFindBack(fun c -> c.HitTest(e.Location))
        match oc with 
            | Some(c) -> 
                let p = c.WV.TransformPointView(PointF(single e.X, single e.Y))
                let evt = new MouseEventArgs(e.Button, e.Clicks, int p.X, int p.Y, e.Delta)
                c.OnMouseDoubleClick(evt)
            | None -> ()

    override this.OnKeyDown(e) =
        controls |> Seq.iter(fun c -> 
            c.OnKeyDown(e)
        )

    override this.OnKeyUp(e) =
        controls |> Seq.iter(fun c ->
            c.OnKeyUp(e)
        )
        
    override this.OnPaint(e:PaintEventArgs) =
        controls |> Seq.iter(fun c -> 
            let bkg = e.Graphics.Save()
            let evt = new PaintEventArgs(e.Graphics, Rectangle(c.PositionInt, c.SizeInt))
            e.Graphics.SetClip(new RectangleF(c.Position, c.Size))
            e.Graphics.Transform <- c.WV.WV
            c.OnPaint(evt)
            e.Graphics.Restore(bkg)
        )

// ======================================================================================================== \\

type Nodo() = 
    
    let mutable arrayArchi = new ResizeArray<Arco>()
    let mutable boundingBox = new RectangleF()
    let mutable selezionato = false
    let mutable text = "empty"

    member this.Centro 
        with get() = 
            let x = boundingBox.X + (boundingBox.Width/2.f)
            let y = boundingBox.Y + (boundingBox.Height/2.f)
            PointF(x,y)
        
    member this.Paint(g:Graphics) =
        if selezionato then 
            g.DrawRectangle(Pens.LightGray, int boundingBox.X, int boundingBox.Y, int boundingBox.Width, int boundingBox.Height)
        g.DrawEllipse(Pens.Black,boundingBox)
        g.DrawString(text, new Font("Arial",11.f), Brushes.Black,boundingBox.X,this.Centro.Y-10.f)

    member this.Size
        with get() = boundingBox.Size
        and set(v) = boundingBox.Size <- v
            
    member this.Location
        with get() = boundingBox.Location
        and set(v) = 
            boundingBox.Location <- v
            arrayArchi |> Seq.iter(fun a ->
                if a.Nodo1.Equals(this) then
                    a.Nodo1 <- this
                else 
                    a.Nodo2 <- this
            )

    member this.IsIn(p:Point) = 
        let pfloat = PointF(single p.X, single p.Y)
        boundingBox.Contains(pfloat)

    member this.Selezionato
        with get() = selezionato
        and set(v) = selezionato <- v 
    
    member this.Text 
        with get() = text
        and set(v) = text <- v            

    member this.Add(a:Arco) =
        arrayArchi.Add(a)

    member this.ArrayArchi with get() = arrayArchi
        
and Arco() = 
    let mutable nodo1 = Nodo()
    let mutable nodo2 = Nodo()
    let mutable selezionato = false
    let mutable boundingBox = RectangleF()
    let mutable text = "empty"
    
    member this.Paint(g:Graphics) =
        if selezionato then
            g.DrawRectangle(Pens.LightGray, int boundingBox.X, int boundingBox.Y, int boundingBox.Width, int boundingBox.Height)
        g.DrawLine(Pens.Black, nodo1.Centro, nodo2.Centro)
        let x = boundingBox.X + (boundingBox.Width/2.f)
        let y = boundingBox.Y + (boundingBox.Height/2.f)
        g.DrawString(text, new Font("Arial",11.f), Brushes.Black,x,y)

    member this.Nodo1
        with get() = nodo1
        and set(v) = 
            nodo1 <- v
            boundingBox.X <- min nodo1.Centro.X nodo2.Centro.X
            boundingBox.Y <- min nodo1.Centro.Y nodo2.Centro.Y
            boundingBox.Width <- Math.Abs(nodo1.Centro.X - nodo2.Centro.X)
            boundingBox.Height <- Math.Abs(nodo1.Centro.Y - nodo2.Centro.Y)
            
    member this.Nodo2
        with get() = nodo2
        and set(v) = 
            nodo2 <- v
            boundingBox.X <- min nodo1.Centro.X nodo2.Centro.X
            boundingBox.Y <- min nodo1.Centro.Y nodo2.Centro.Y
            boundingBox.Width <- Math.Abs(nodo1.Centro.X - nodo2.Centro.X)
            boundingBox.Height <- Math.Abs(nodo1.Centro.Y - nodo2.Centro.Y)

    member this.Selezionato 
        with get() = selezionato
        and set(v) = selezionato <- v

    member this.IsIn(p:Point) =
        let point = PointF(single p.X, single p.Y)
        boundingBox.Contains(point)

    member this.Text
        with get() = text
        and set(v) = text <- v

// ======================================================================================================== \\

type LWButton(g:LWGrafo,cdg:LWCompositoreDiGrafo) as this = 
    inherit LWCControl()

    do
        this.Size <- SizeF(75.f, 20.f)

    let mutable text = "bottone"

    member this.Text 
        with get() = text 
        and set(v) = 
            text <- v 
            this.Invalidate()

    override this.OnPaint(e) =
        let g = e.Graphics
        g.FillRectangle(Brushes.LightGray, 0.f, 0.f, this.Width, this.Height)
        g.DrawRectangle(Pens.Black, 0.f, 0.f, this.Width, this.Height)
        g.DrawString(text, new Font("Arial",8.f), Brushes.Black, PointF(5.f, 5.f))

    override this.OnMouseDown(e) = 
        match text with 
            | "Up" -> 
                g.WV.TranslateView(0.f, 10.f)
                cdg.Invalidate()
            | "Down" -> 
                g.WV.TranslateView(0.f, -10.f)
                cdg.Invalidate()
            | "Zoom In" -> 
                g.WV.ScaleWorld(single 1.1, single 1.1)
                cdg.Invalidate()
            | "Zoom Out" -> 
                g.WV.ScaleWorld(single 1/single 1.1, single 1/single 1.1)
                cdg.Invalidate()
            | "Rotate Dx" -> 
                g.WV.RotateView(-5.f)
                cdg.Invalidate()
            | "Rotate Sx" -> 
                g.WV.RotateView(5.f)
                cdg.Invalidate()
            | _ -> ()

and LWGrafo(cdg:LWCompositoreDiGrafo) as this = 
    inherit LWCControl()

    let mutable arrayNodi = new ResizeArray<Nodo>()
    let mutable arrayArchi = new ResizeArray<Arco>()
    let mutable dragNodo = None
    let mutable isCtrl = false
    let mutable isSelezionato = None
    let updateText (s:string) =
        let no = arrayNodi |> Seq.tryFind(fun n -> n.Selezionato)
        let ar = arrayArchi |> Seq.tryFind(fun a -> a.Selezionato)
        match no with
            | Some nodo -> 
                nodo.Text <- nodo.Text + s
                cdg.Invalidate()
            | None -> ()
        match ar with
            | Some arco -> 
                arco.Text <- arco.Text + s
                cdg.Invalidate()
            | None -> ()

    do
        this.Size <- SizeF(800.f, 800.f)

    override this.OnPaint(e) =

        let g = e.Graphics
        g.SmoothingMode <- Drawing2D.SmoothingMode.HighQuality
        g.Transform <- this.WV.WV

        arrayNodi |> Seq.iter(fun n -> n.Paint(g))
        arrayArchi |> Seq.iter(fun a -> a.Paint(g))
        
    override this.OnMouseDoubleClick(e) =
        arrayNodi.Add(Nodo(Size = SizeF(50.f,50.f), Location = PointF(single e.Location.X-25.f, single e.Location.Y-25.f)))
        arrayNodi |> Seq.iter(fun n -> n.Selezionato <- false)
        arrayArchi |> Seq.iter(fun a -> a.Selezionato <- false)
        cdg.Invalidate()

    override this.OnMouseDown(e) =
        if isCtrl then
            isSelezionato <- arrayNodi |> Seq.tryFind(fun n -> n.Selezionato)            

        match (isSelezionato,isCtrl) with
            | (Some nodo1,true) -> 
                let n2 = arrayNodi |> Seq.tryFind(fun n -> n.IsIn(e.Location))
                match n2 with
                    | Some nodo2 -> 
                        let arco = Arco(Nodo1 = nodo1, Nodo2 = nodo2)
                        arrayArchi.Add(arco)
                        nodo1.Add(arco)
                        nodo2.Add(arco)
                        isSelezionato <- None
                        cdg.Invalidate()
                    | None -> 
                        arrayNodi |> Seq.iter(fun n -> n.Selezionato <- false)
                        arrayArchi |> Seq.iter(fun a -> a.Selezionato <- false)
                        cdg.Invalidate()

            | (_,_) ->
                let nodo = arrayNodi |> Seq.tryFind(fun n -> n.IsIn(e.Location))
                let arco = arrayArchi |> Seq.tryFind(fun a -> a.IsIn(e.Location))
                match nodo with
                    | Some n -> 
                        dragNodo <- Some(n, e.X - int n.Location.X, e.Y - int n.Location.Y)
                        arrayNodi |> Seq.iter(fun n -> n.Selezionato <- false)
                        n.Selezionato <- true
                        cdg.Invalidate()
                    | None -> 
                        arrayNodi |> Seq.iter(fun n -> n.Selezionato <- false)
                        cdg.Invalidate()
                match arco with
                    | Some(a) ->
                        arrayArchi |> Seq.iter(fun a -> a.Selezionato <- false)
                        a.Selezionato <- true
                        cdg.Invalidate()
                    | None -> 
                        arrayArchi |> Seq.iter(fun a -> a.Selezionato <- false)
                        cdg.Invalidate()

    override this.OnMouseMove(e) = 
        match dragNodo with 
            | Some(nodo, dx ,dy) ->
                nodo.Location <- PointF(single(e.X - dx), single(e.Y - dy))
                cdg.Invalidate()
            | _ -> ()
        
    override this.OnMouseUp(e) = 
        dragNodo <- None

    override this.OnKeyDown(e) =
        
        isCtrl <- e.Control
        
        match e.KeyData with
            | Keys.Delete -> 
                let nodo = arrayNodi |> Seq.tryFind(fun n -> n.Selezionato)
                let arco = arrayArchi |> Seq.tryFind(fun a -> a.Selezionato)
                match nodo with
                    | Some(n) -> 
                        arrayNodi.Remove(n) |> ignore
                        n.ArrayArchi |> Seq.iter(fun a -> arrayArchi.Remove(a) |> ignore)
                        cdg.Invalidate()
                    | None -> ()
                match arco with
                    | Some(a) -> 
                        arrayArchi.Remove(a) |> ignore
                        a.Nodo1.ArrayArchi.Remove(a) |> ignore
                        a.Nodo2.ArrayArchi.Remove(a) |> ignore
                        cdg.Invalidate()
                    | None -> ()
            | Keys.D0 -> updateText "0"
            | Keys.D1 -> updateText "1"
            | Keys.D2 -> updateText "2"
            | Keys.D3 -> updateText "3"
            | Keys.D4 -> updateText "4"
            | Keys.D5 -> updateText "5"
            | Keys.D6 -> updateText "6"
            | Keys.D7 -> updateText "7"
            | Keys.D8 -> updateText "8"
            | Keys.D9 -> updateText "9"
            | Keys.A -> updateText "a"
            | Keys.B -> updateText "b"
            | Keys.C -> updateText "c"
            | Keys.D -> updateText "d"
            | Keys.E -> updateText "e"
            | Keys.F -> updateText "f"
            | Keys.G -> updateText "g"
            | Keys.H -> updateText "h"
            | Keys.I -> updateText "i"
            | Keys.L -> updateText "l"
            | Keys.M -> updateText "m"
            | Keys.N -> updateText "n"
            | Keys.O -> updateText "o"
            | Keys.P -> updateText "p"
            | Keys.Q -> updateText "q"
            | Keys.R -> updateText "r"
            | Keys.S -> updateText "s"
            | Keys.T -> updateText "t"
            | Keys.U -> updateText "u"
            | Keys.V -> updateText "v"
            | Keys.Z -> updateText "z"
            | Keys.W -> updateText "w"
            | Keys.Y -> updateText "y"
            | Keys.J -> updateText "j"
            | Keys.K -> updateText "k"
            | Keys.X -> updateText "x"
            | Keys.Back -> 
                let no = arrayNodi |> Seq.tryFind(fun n -> n.Selezionato)
                let ar = arrayArchi |> Seq.tryFind(fun a -> a.Selezionato)
                match no with
                    | Some nodo -> 
                        nodo.Text <- nodo.Text.Remove(nodo.Text.Length-1)
                        cdg.Invalidate()
                    | None -> ()
                match ar with
                    | Some arco ->
                        arco.Text <- arco.Text.Remove(arco.Text.Length-1)
                        cdg.Invalidate()
                    | None -> ()
            | _ -> ()

    override this.OnKeyUp(e) =
        isCtrl <- e.Control

and LWCompositoreDiGrafo() as this =
    inherit LWCControl()

    let lwcc = new LWCContainer(Dock = DockStyle.Fill)
    let grafo = new LWGrafo(this)
    let btnU = new LWButton(grafo, this, Text = "Up", Position = PointF(5.f,5.f))
    let btnD = new LWButton(grafo, this, Text = "Down", Position = PointF(5.f,10.f + btnU.Height))
    let btnI = new LWButton(grafo, this, Text = "Zoom In", Position = PointF(10.f+btnU.Width,5.f))
    let btnO = new LWButton(grafo, this, Text = "Zoom Out", Position = PointF(10.f+btnU.Width,10.f + btnU.Height))
    let btnDx = new LWButton(grafo, this, Text = "Rotate Dx", Position = PointF(15.f+btnU.Width+btnU.Width,5.f))
    let btnDy = new LWButton(grafo, this, Text = "Rotate Sx", Position = PointF(15.f+btnU.Width+btnU.Width,10.f + btnU.Height))

    do
        this.Size <- SizeF(800.f, 800.f)
        lwcc.LWControls.Add(grafo)
        lwcc.LWControls.Add(btnD)
        lwcc.LWControls.Add(btnU)
        lwcc.LWControls.Add(btnI)
        lwcc.LWControls.Add(btnO)
        lwcc.LWControls.Add(btnDx)
        lwcc.LWControls.Add(btnDy)

    override this.OnMouseDown(e) =
        let oc = lwcc.LWControls |> Seq.tryFindBack(fun c -> c.HitTest(e.Location))
        match oc with 
        | Some c -> 
            let p = c.WV.TransformPointView(PointF(single e.X, single e.Y))
            let evt = new MouseEventArgs(e.Button, e.Clicks, int p.X, int p.Y, e.Delta)
            c.OnMouseDown(evt)
        | None -> ()

    override this.OnMouseUp(e) =
        let oc = lwcc.LWControls |> Seq.tryFindBack(fun c -> c.HitTest(e.Location))
        match oc with 
        | Some c -> 
            let p = c.WV.TransformPointView(PointF(single e.X, single e.Y))
            let evt = new MouseEventArgs(e.Button, e.Clicks, int p.X, int p.Y, e.Delta)
            c.OnMouseUp(evt)
        | None -> ()

    override this.OnMouseMove(e) =
        let oc = lwcc.LWControls |> Seq.tryFindBack(fun c -> c.HitTest(e.Location))
        match oc with 
        | Some c -> 
            let p = c.WV.TransformPointView(PointF(single e.X, single e.Y))
            let evt = new MouseEventArgs(e.Button, e.Clicks, int p.X, int p.Y, e.Delta)
            c.OnMouseMove(evt)
        | None -> ()

    override this.OnMouseDoubleClick(e) =
        let oc = lwcc.LWControls |> Seq.tryFindBack(fun c -> c.HitTest(e.Location))
        match oc with 
            | Some(c) -> 
                let p = c.WV.TransformPointView(PointF(single e.X, single e.Y))
                let evt = new MouseEventArgs(e.Button, e.Clicks, int p.X, int p.Y, e.Delta)
                c.OnMouseDoubleClick(evt)
            | None -> ()

    override this.OnKeyDown(e) =
        lwcc.LWControls |> Seq.iter(fun c -> 
            c.OnKeyDown(e)
        )

    override this.OnKeyUp(e) =
        lwcc.LWControls |> Seq.iter(fun c ->
            c.OnKeyUp(e)
        )
        
    override this.OnPaint(e:PaintEventArgs) =
        lwcc.LWControls |> Seq.iter(fun c -> 
            let bkg = e.Graphics.Save()
            let evt = new PaintEventArgs(e.Graphics, Rectangle(c.PositionInt, c.SizeInt))
            e.Graphics.SetClip(new RectangleF(c.Position, c.Size))
            e.Graphics.Transform <- c.WV.WV
            c.OnPaint(evt)
            e.Graphics.Restore(bkg)
        )

// ======================================================================================================== \\
    
let f = new Form(TopMost = true, Text = "Compositore Di Grafi")
let lwcc = new LWCContainer(Dock = DockStyle.Fill)
let cdg = new LWCompositoreDiGrafo()

lwcc.LWControls.Add(cdg)
f.Controls.Add(lwcc)
f.Show()