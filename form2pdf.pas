unit form2pdf;

{$mode objfpc}{$H+}

{Copyright Alan Chamberlain 2020

This unit renders the text and image components of a form to a PDF using the
fcl-pdf package. The object is not to provide a pixel by pixel representation of
the form, but to record the text and image information. Multiline controls such
as TStringGrid and TMemo are printed out in their entirety. the unit is
modularised so new components can be added easily. Supported components are:
TForm
TLabel
TStaticText
TEdit
TSpinEdit
TFloatSpinEdit
TSpinEditEx
TFloatSpinEditEx
TDirectoryEdit
TFileNameEdit
TComboBox
TListBox
TStringGrid
TValueListEditor
TMemo (does not support word wrapping)
TCheckBox
TRadioButton
TImage
TChart
TShape (rectangle, rounded rect, ellipse)
TPageControl
TTabSheet
TGroupBox
TPanel
TRadioGroup
TCheckGroup

To use copy form2pdf.pas into your source directory and include Form2PDF in
your uses clause. Any visual control can be passed as a parent eg. TTabControl.

Licence: Apache v2.0

History
26/6/2020 Initial commit.
3/7/2020  Fix bottom margin pagination.
5/07/2020 (TvS):moved initialization of FormToPDF to initalization part of unit
6/7/2020  changed FormToPDF to function to return error code
          added control and filename checks
8/7/2020  add functionality to append pages to document, FDoc now global}

interface

uses
   Classes, Forms, Graphics, Controls, SysUtils, fppdf;

var FDoc       :TPDFDocument;  {declare FDOC global so it can be accessed by caller}

function FormToPDF:integer;                     {initialise FDoc and check if fonts are available}
function FormToPDF(AControl: TControl):integer; {parse controls and append pages to Fdoc}
function FormToPDF(FileName:string):integer;    {use to save FDoc to PDF and reset FDoc}
function FormToPDF(AControl:TControl; FileName:string):integer; {parse controls and save and close Fdoc}

implementation

uses StdCtrls, ExtCtrls, ComCtrls,TAGraph, Grids, Spin, SpinEx, EditBtn,ValEdit,
     fpparsettf, fpttf, intfgraphics;

procedure RecurseControls(AControl:TControl; FDoc:TPDFDocument; Page:TPDFPage; ftText,ML,MT:integer); forward;
procedure ParseControls(AControl:TControl; FDoc:TPDFDocument; Page:TPDFPage; ftText,ML,MT:integer); forward;

var FirstPage  :boolean;
    CustomPaper:TPDFPaper;

{------------------------------------------------------------------------------
Page and Document set up
------------------------------------------------------------------------------}

function FontsAvailable: Boolean;
begin
Result:= (gTTFontCache.Count > 0);
end;


function SetupPage(AControl:TControl; FDoc:TPDFDocument; ML,MT:integer):TPDFPage;
var APage      :TPDFPage;

begin
APage := FDoc.Pages.AddPage;
if FDoc.DefaultPaperType = ptCustom then
   APage.Paper := CustomPaper;
APage.PaperType := FDoc.DefaultPaperType;
if APage.Orientation = ppoPortrait then     {work around to trigger adjustmatrix}
   begin
   APage.Orientation := ppoLandscape;
   APage.Orientation := ppoPortrait;
   end
  else
   begin
   APage.Orientation := ppoPortrait;
   APage.Orientation := ppoLandscape;
   end;

FDoc.Sections[0].AddPage(APage);            {only created one section}
Result := APage;
end;


function ColorToPDF(AColor:TColor):TARGBColor;
{Red and blue values appear to be swapped}
var c:         TColor;
begin
c := ColorToRGB(AColor);
Result := Red(c)*$10000 + Green(c)*$100 + Blue(c);
end;


function GetPDFPenStyle(APen:TPen):TPDFPenStyle;
{Unfortunately mapping is not 1:1 therefore this clumsy method}
begin
Result := ppsSolid;
case APen.Style of
   psDash:       Result := ppsDash;
   psDot:        Result := ppsDot;
   psDashDot:    Result := ppsDashDot;
   psDashDotDot: Result := ppsDashDotDot;
   end; {of case}
end;


procedure Header2PDF(cForm:Tform; APage:TPDFPage; IDX,ML,MT:integer);
var DX,DY,                     {x and y pos to draw item}
    DW         :integer;       {height and width to draw item}
    lFC        :TFPFontCacheItem;{font item}
    FtName     :string;

begin
FtName := 'FreeSans';
lFC := gTTFontCache.Find(FtName, True, False);
if not Assigned(lFC) then
   raise Exception.Create(FtName + ' font not found');
DW := Round(lFC.TextWidth(cForm.Caption,24));
DX := (APage.Paper.W - DW) div 2;
DY := (MT - 24) div 2 + 24;                        {put caption halfway in header}
APage.SetFont(IDX, 24);
APage.SetColor(cForm.Font.Color, false);
APage.WriteText(DX,DY,cForm.Caption);
end;


procedure DrawVarBorder(AControl:TControl; APage:TPDFPage; DX,DY,ML,MT:integer);
{draw rectangle around border}
var DW,DH      :integer;       {height and width to draw item}
begin
DW := AControl.Width;
DH := DY - (MT + AControl.Top);
DX := ML + AControl.Left;
DY := MT + AControl.Top + DH;
APage.DrawRect(DX,DY,DW,DH,1,false,true);
end;


procedure DrawFixedBorder(AControl:TControl; APage:TPDFPage; ML,MT:integer);
var DX,DY,                     {position of item}
    DW,DH      :integer;       {height and width to draw item}
    Isfilled   :boolean;       {is the shape filled}

begin
IsFilled := false;
{draw rectangle around border}
DW := AControl.Width;
DH := AControl.Height;
DX := ML + AControl.Left;
DY := MT + AControl.Top + DH;
if AControl.Color <> clDefault then
   begin
   APage.SetColor(ColorToPDF(AControl.Color),false);
   IsFilled := true;
   end;
APage.DrawRect(DX,DY,DW,DH,1,IsFilled,true);
end;


function SetControlFont(AControl:TControl; APage:TPDFPage; IDX:integer):integer;
var fsize      :integer;       {font size}
begin
fSize := abs(GetFontData(AControl.Font.Handle).Height);
APage.SetColor(ColorToPDF(AControl.Font.Color),false);
APage.SetFont(IDX,fSize);
Result := fsize;
end;

{------------------------------------------------------------------------------
Component Procedures
------------------------------------------------------------------------------}

{TLabel}
procedure LabelToPDF(cLabel:TLabel; APage:TPDFPage; IDX,ML,MT:integer);
var fSize,                     {font size}
    DX,DY,                     {x and y pos to draw item}
    DH         :integer;       {height and width to draw item}
begin
if cLabel.Visible then
   begin
   fsize := SetControlFont(cLabel,APage,IDX);
   DH := cLabel.Height;
   DX := ML + cLabel.Left;
   DY := MT + cLabel.Top + DH;
   APage.WriteText(DX,DY,cLabel.Caption);
   end;
end;


{TStaticText}
procedure StaticTextToPDF(cLabel:TStaticText; APage:TPDFPage; IDX,ML,MT:integer);
var fSize,                     {font size}
    DX,DY,                     {x and y pos to draw item}
    DH         :integer;       {height and width to draw item}
begin
if cLabel.Visible then
   begin
   fsize := SetControlFont(cLabel,APage,IDX);
   DH := cLabel.Height;
   DX := ML + cLabel.Left;
   DY := MT + cLabel.Top + DH;
   APage.WriteText(DX,DY,cLabel.Caption);
   end;
end;


{TImage}
procedure ImageToPDF(cImage:TImage; ADoc:TPDFDocument; APage:TPDFPage; ML,MT:integer);
var IDX,
    DX,DY,                     {x and y pos to draw item}
    DH,DW      :integer;       {height and width to draw item}
    pdfImage   :TPDFImageItem;
    fpBitmap   :TLazIntfImage;
begin
if cImage.Visible then
   begin
   pdfImage := ADoc.Images.AddImageItem;
   IDX := ADoc.Images.Count - 1;
   fpBitMap := TLazIntfImage.Create(0,0);
   fpBitMap.LoadFromBitmap(cImage.Picture.Bitmap.Handle,cImage.Picture.Bitmap.MaskHandle);
   pdfImage.Image := fpBitMap;
   pdfImage.OwnsImage := true;
   DW := cImage.Width;
   DH := cImage.Height;
   DX := ML + cImage.Left;
   DY := MT + cImage.Top + DH;
   APage.DrawImageRawSize(DX,DY,DW,DH,IDX);
   end;
end;


{TChart}
procedure ChartToPDF(cChart:TChart; ADoc:TPDFDocument; APage:TPDFPage; ML,MT:integer);
{for now copy chart to bitmap. use fpVectorial later?}
var IDX,
    DX,DY,                     {x and y pos to draw item}
    DH,DW      :integer;       {height and width to draw item}
    pdfImage   :TPDFImageItem;
    fpBitmap   :TLazIntfImage;
    BitMap     :TBitMap;
begin
if cChart.Visible then
   begin
   pdfImage := ADoc.Images.AddImageItem;
   IDX := ADoc.Images.Count - 1;
   fpBitMap := TLazIntfImage.Create(0,0);
   BitMap := cChart.SaveToImage(TBitMap) as TBitMap;
   fpBitmap.LoadFromBitmap(BitMap.Handle,BitMap.MaskHandle);
   pdfImage.Image := fpBitMap;
   pdfImage.OwnsImage := true;
   DW := cChart.Width;
   DH := cChart.Height;
   DX := ML + cChart.Left;
   DY := MT + cChart.Top + DH;
   APage.DrawImageRawSize(DX,DY,DW,DH,IDX);
   BitMap.Free;
   end;
end;


{TShape}
procedure ShapeToPDF(cShape:TShape; APage:TPDFPage; ML,MT:integer);
{Supported shapes:
Rectangle
Rounded Rect
Ellipse
}
var LW,
    DX,DY,                     {x and y pos to draw item}
    DH,DW      :integer;       {height and width to draw item}
    Isfilled   :boolean;       {is the shape filled}

begin
if cShape.Visible then
   begin
   Isfilled := true;
   APage.SetColor(ColorToPDF(cShape.Pen.Color),true);
   APage.SetPenStyle(GetPDFPenStyle(cShape.Pen));
   APage.SetColor(ColorToPDF(cShape.Brush.Color),false);
   if cShape.Brush.Style = bsClear then Isfilled := false;
   DW := cShape.Width;
   DH := cShape.Height;
   DX := ML + cShape.Left;
   DY := MT + cShape.Top + DH;
   LW := cShape.Pen.Width;
   case cShape.Shape of
      stRectangle: APage.DrawRect(DX,DY,DW,DH,LW,Isfilled,true);
      stRoundRect: APage.DrawRoundedRect(DX,DY,DW,DH,(DW/20 + DH/20),LW,Isfilled,true);
      stEllipse  : APage.DrawEllipse(DX,DY,DW,DH,LW,IsFilled,true);
      end; {case}
   end;
end;


{TGroupBox}
procedure GroupBoxToPDF(cGroupBx:TGroupBox; FDoc:TPDFDocument; APage:TPDFPage; IDX,ML,MT:integer);
var I,
    fSize,                     {font size}
    DX,DY      :integer;       {x and y pos to draw item}
begin
if cGroupBx.Visible then
   begin
   DrawFixedBorder(cGroupBx,APage,ML,MT);

   {write groupbox caption}
   fsize := SetControlFont(cGroupBx,APage,IDX);
   DX := ML + cGroupBx.Left + 2;
   DY := MT + cGroupBx.Top + fSize + 2;
   APage.WriteText(DX,DY,cGroupBx.Caption);

   {draw components}
   ML := ML + cGroupBx.Left;
   MT := MT + cGroupBx.Top + cGroupBx.Height - cGroupBx.ClientHeight;
   for I:=0 to cGroupBx.ControlCount - 1 do
      ParseControls(cGroupBx.Controls[I],FDoc,APage,IDX,ML,MT);
   end;
end;


{TPanel}
procedure PanelToPDF(cPanel:TPanel; FDoc:TPDFDocument; APage:TPDFPage; IDX,ML,MT:integer);
var I          :integer;
begin
if cPanel.Visible then
   begin
   {for now don't write caption}
   DrawFixedBorder(cPanel,APage,ML,MT);

   {draw components}
   ML := ML + cPanel.Left;
   MT := MT + cPanel.Top + cPanel.Height - cPanel.ClientHeight;
   for I:=0 to cPanel.ControlCount - 1 do
      ParseControls(cPanel.Controls[I],FDoc,APage,IDX,ML,MT);
   end;
end;


{TEdit}
procedure EditToPDF(cEdit:TEdit; APage:TPDFPage; IDX,ML,MT:integer);
var fSize,                     {font size}
    DX,DY,                     {x and y pos to draw item}
    DH         :integer;       {height and width to draw item}
begin
if cEdit.Visible then
   begin
   DrawFixedBorder(cEdit,APage,ML,MT);

   {write edit caption}
   fsize := SetControlFont(cEdit,APage,IDX);
   DH := cEdit.Height;
   DX := ML + cEdit.Left;
   DY := MT + cEdit.Top + fSize + (DH - fSize) div 2;
   APage.WriteText(DX + 2,DY,cEdit.Caption);
   end;
end;

{Note: TspinEdit, TFloatSpinEdit, TSpinEditEx and TFloatSpinEditEx are programatically
the same, but if the -CR compiler switch is set we can only typecast to the exact
type. Base class of TSpinEditEx and TFloatSpinEditEx is not specialised so we
cannot typecast to it.}

{TCustomFloatSpinEdit}
procedure CustomFloatSpinEditToPDF(cSpinEd:TCustomFloatSpinEdit; APage:TPDFPage; IDX,ML,MT:integer);
var fSize,                     {font size}
    DX,DY,                     {x and y pos to draw item}
    DH,                        {height to draw item}
    X1,X2,
    Y1,Y2      :integer;

begin
if cSpinEd.Visible then
   begin
   DrawFixedBorder(cSpinEd,APage,ML,MT);

   {write edit caption}
   fsize := SetControlFont(cSpinEd,APage,IDX);
   DH := cSpinEd.Height;
   DX := ML + cSpinEd.Left;
   DY := MT + cSpinEd.Top + fSize + (DH - fSize) div 2;
   APage.WriteText(DX + 2,DY,cSpinEd.Caption);

   {draw up tick}
   X1 := DX + cSpinEd.Width - DH div 2 + 2;
   Y1 := MT + cSpinEd.Top + 2;
   X2 := X1 + DH div 4 - 2;
   Y2 := Y1 + DH div 3 - 2;
   APage.DrawLine(X1,Y2,X2,Y1,1,true);
   APage.DrawLine(X1 + DH div 4 - 2,Y1,X2 + DH div 4 - 2,Y2,1,true);

   {draw down tick}
   Y1 := MT + cSpinEd.Top + DH - 2;
   Y2 := Y1 - DH div 3 + 2;
   APage.DrawLine(X1,Y2,X2,Y1,1,true);
   APage.DrawLine(X1 + DH div 4 - 2,Y1,X2 + DH div 4 - 2,Y2,1,true);
   end;
end;


{TSpinEdit}
procedure SpinEditToPDF(cSpinEd:TSpinEdit; APage:TPDFPage; IDX,ML,MT:integer);
begin
CustomFloatSpinEditToPDF(TCustomFloatSpinEdit(cSpinEd),APage,IDX,ML,MT)
end;


{TFloatSpinEdit}
procedure FloatSpinEditToPDF(cSpinEd:TFloatSpinEdit; APage:TPDFPage; IDX,ML,MT:integer);
begin
CustomFloatSpinEditToPDF(TCustomFloatSpinEdit(cSpinEd),APage,IDX,ML,MT)
end;


{TSpinEditEx}
procedure SpinEditExToPDF(cSpinEd:TSpinEditEx; APage:TPDFPage; IDX,ML,MT:integer);
var fSize,                     {font size}
    DX,DY,                     {x and y pos to draw item}
    DH,                        {height to draw item}
    X1,X2,
    Y1,Y2      :integer;

begin
if cSpinEd.Visible then
   begin
   DrawFixedBorder(cSpinEd,APage,ML,MT);

   {write edit caption}
   fsize := SetControlFont(cSpinEd,APage,IDX);
   DH := cSpinEd.Height;
   DX := ML + cSpinEd.Left;
   DY := MT + cSpinEd.Top + fSize + (DH - fSize) div 2;
   APage.WriteText(DX + 2,DY,cSpinEd.Caption);

   {draw up tick}
   X1 := DX + cSpinEd.Width - DH div 2 + 2;
   Y1 := MT + cSpinEd.Top + 2;
   X2 := X1 + DH div 4 - 2;
   Y2 := Y1 + DH div 3 - 2;
   APage.DrawLine(X1,Y2,X2,Y1,1,true);
   APage.DrawLine(X1 + DH div 4 - 2,Y1,X2 + DH div 4 - 2,Y2,1,true);

   {draw down tick}
   Y1 := MT + cSpinEd.Top +DH - 2;
   Y2 := Y1 - DH div 3 + 2;
   APage.DrawLine(X1,Y2,X2,Y1,1,true);
   APage.DrawLine(X1 + DH div 4 - 2,Y1,X2 + DH div 4 - 2,Y2,1,true);
   end;
end;


{TFloatSpinEditEx}
procedure FloatSpinEditExToPDF(cSpinEd:TFloatSpinEditEx; APage:TPDFPage; IDX,ML,MT:integer);
var fSize,                     {font size}
    DX,DY,                     {x and y pos to draw item}
    DH,                        {height to draw item}
    X1,X2,
    Y1,Y2      :integer;

begin
if cSpinEd.Visible then
   begin
   DrawFixedBorder(cSpinEd,APage,ML,MT);

   {write edit caption}
   fsize := SetControlFont(cSpinEd,APage,IDX);
   DH := cSpinEd.Height;
   DX := ML + cSpinEd.Left;
   DY := MT + cSpinEd.Top + fSize + (DH - fSize) div 2;
   APage.WriteText(DX + 2,DY,cSpinEd.Caption);

   {draw up tick}
   X1 := DX + cSpinEd.Width - DH div 2 + 2;
   Y1 := MT + cSpinEd.Top + 2;
   X2 := X1 + DH div 4 - 2;
   Y2 := Y1 + DH div 3 - 2;
   APage.DrawLine(X1,Y2,X2,Y1,1,true);
   APage.DrawLine(X1 + DH div 4 - 2,Y1,X2 + DH div 4 - 2,Y2,1,true);

   {draw down tick}
   Y1 := MT + cSpinEd.Top +DH - 2;
   Y2 := Y1 - DH div 3 + 2;
   APage.DrawLine(X1,Y2,X2,Y1,1,true);
   APage.DrawLine(X1 + DH div 4 - 2,Y1,X2 + DH div 4 - 2,Y2,1,true);
   end;
end;


{TDirectoryEdit}
procedure DirEditToPDF(cDirEdit:TDirectoryEdit; APage:TPDFPage; IDX,ML,MT:integer);
var fSize,                     {font size}
    DX,DY,                     {x and y pos to draw item}
    DH         :integer;       {height and width to draw item}
begin
if cDirEdit.Visible then
   begin
   DrawFixedBorder(cDirEdit,APage,ML,MT);

   {write edit caption}
   fsize := SetControlFont(cDirEdit,APage,IDX);
   DH := cDirEdit.Height;
   DX := ML + cDirEdit.Left;
   DY := MT + cDirEdit.Top + fSize + (DH - fSize) div 2;
   APage.WriteText(DX + 2,DY,cDirEdit.Caption);
   end;
end;


{TFileNameEdit}
procedure FileEditToPDF(cFileEdit:TFileNameEdit; APage:TPDFPage; IDX,ML,MT:integer);
var fSize,                     {font size}
    DX,DY,                     {x and y pos to draw item}
    DH         :integer;       {height and width to draw item}
begin
if cFileEdit.Visible then
   begin
   DrawFixedBorder(cFileEdit,APage,ML,MT);

   {write edit caption}
   fsize := SetControlFont(cFileEdit,APage,IDX);
   DH := cFileEdit.Height;
   DX := ML + cFileEdit.Left;
   DY := MT + cFileEdit.Top + fSize + (DH - fSize) div 2;
   APage.WriteText(DX + 2,DY,cFileEdit.Caption);
   end;
end;


{TComboBox}
procedure ComboBoxToPDF(cCmboBx:TComboBox; APage:TPDFPage; IDX,ML,MT:integer);
{prints selected text only, not the drop down list}
var fSize,                     {font size}
    DX,DY,                     {x and y pos to draw item}
    DW,DH,                     {height and width to draw item}
    X1,X2,
    Y1,Y2      :integer;

begin
if cCmboBx.Visible then
   begin
   DrawFixedBorder(cCmboBx,APage,ML,MT);

   {write edit caption}
   fsize := SetControlFont(cCmboBx,APage,IDX);
   DH := cCmboBx.Height;
   DW := cCmboBx.Width;
   DX := ML + cCmboBx.Left;
   DY := MT + cCmboBx.Top + fSize + (DH - fSize) div 2;
   APage.WriteText(DX + 2,DY,cCmboBx.Caption);

   {draw down tick}
   X1 := DX + DW - DH + 2;
   Y1 := MT + cCmboBx.Top + 2*DH div 3;
   X2 := X1 + DH div 2 - 2;
   Y2 := Y1 - DH div 3 + 2;
   APage.DrawLine(X1,Y2,X2,Y1,1,true);
   APage.DrawLine(X1 + DH div 2 - 2,Y1,X2 + DH div 2 - 2,Y2,1,true);
   end;
end;


{TMemo}
procedure MemoToPDF(cMemo:TMemo; FDoc:TPDFDocument; APage:TPDFPage; IDX,ML,MT:integer);
var I,
    fSize,                     {font size}
    DX,DY      :integer;       {x and y pos to draw item}
    fp         :boolean;       {first page of control}

begin
if cMemo.Visible then
   begin
   {write text, no line wrapping}
   fp := true;
   fsize := SetControlFont(cMemo,APage,IDX);
   DX := ML + cMemo.Left + 2;
   DY := MT + cMemo.Top + fsize + 2;
   for I:=0 to cMemo.Lines.Count - 1 do
      begin
      APage.WriteText(DX,DY,cMemo.Lines[I]);
      DY := DY + fSize;
      if (DY > APage.Paper.H - MT) or ((DY > MT + cMemo.Top + cMemo.Height) and fp) then
         begin
         DrawVarBorder(cMemo,APage,DX,DY,ML,MT);
         APage := SetupPage(cMemo,FDoc,ML,MT);
         fsize := SetControlFont(cMemo,APage,IDX);
         DY := MT + fsize + 2;
         fp := false;
         end;
      end;
   DrawVarBorder(cMemo,APage,DX,DY,ML,MT);
   end;
end;


{TListBox}
procedure ListBoxToPDF(cLstBx:TListBox; FDoc:TPDFDocument; APage:TPDFPage; IDX,ML,MT:integer);
var I,
    fSize,                     {font size}
    DX,DY      :integer;       {x and y pos to draw item}
    fp         :boolean;       {first page of control}

begin
if cLstBx.Visible then
   begin
   fp := true;
   fsize := SetControlFont(cLstBx,APage,IDX);
   DX := ML + cLstBx.Left + 2;
   DY := MT + cLstBx.Top + fsize + 2;
   for I:=0 to cLstBx.Items.Count - 1 do
      begin
      APage.WriteText(DX,DY,cLstBx.Items[I]);
      DY := DY + fSize;
      if (DY > APage.Paper.H - MT) or ((DY > MT + cLstBx.Top + cLstBx.Height) and fp) then
         begin
         DrawVarBorder(cLstBx,APage,DX,DY,ML,MT);
         APage := SetupPage(cLstBx,FDoc,ML,MT);
         fsize := SetControlFont(cLstBx,APage,IDX);
         DY := MT + cLstBx.Top + fsize + 2;
         fp := false;
         end;
      end;

   DrawVarBorder(cLstBx,APage,DX,DY,ML,MT);
   end;
end;


{TStringGrid}
procedure StringGridToPDF(cStrGrd:TStringGrid; FDoc:TPDFDocument; APage:TPDFPage; IDX,ML,MT:integer);
var I,J,
    fSize,                     {font size}
    DX,DY      :integer;       {x and y pos to draw item}
    fp         :boolean;       {first page of control}

begin
if cStrGrd.Visible then
   begin
   fp := true;
   fsize := SetControlFont(cStrGrd,APage,IDX);
   DX := ML + cStrGrd.Left + 2;
   DY := MT + cStrGrd.Top + fsize + 2;
   for I:=0 to cStrGrd.RowCount - 1 do
      begin
      for J:=0 to cStrGrd.ColCount - 1 do
         begin
         {if column headers are defined write these}
         if (I=0) and (J>0) and (cStrGrd.Columns.Count >= J) and (cStrGrd.FixedRows > 0) then
            APage.WriteText(DX,DY,cStrGrd.Columns[J-1].Title.Caption)
           else
            APage.WriteText(DX,DY,cStrGrd.Cells[J,I]);
         DX := DX + cStrGrd.ColWidths[J];
         end;
      DX := ML + cStrGrd.Left + 2;
      DY := DY + cStrGrd.RowHeights[I];
      if (DY > APage.Paper.H - MT) or ((DY > MT + cStrGrd.Top + cStrGrd.Height) and fp) then
         begin
         DrawVarBorder(cStrGrd,APage,DX,DY,ML,MT);
         APage := SetupPage(cStrGrd,FDoc,ML,MT);
         fsize := SetControlFont(cStrGrd,APage,IDX);
         DY := MT + cStrGrd.Top + fsize + 2;
         fp := false;
         end;
      end;
   DrawVarBorder(cStrGrd,APage,DX,DY,ML,MT);
   end;
end;


{TValueListEditor}
procedure ValueListToPDF(cValueList:TValueListEditor; FDoc:TPDFDocument; APage:TPDFPage; IDX,ML,MT:integer);
var I,SI,
    fSize,                     {font size}
    DX,DY      :integer;       {x and y pos to draw item}
    fp         :boolean;       {first page of control}
    AKey,
    AValue     :string;

begin
if cValueList.Visible then
   begin
   fp := true;
   SI := 0;
   fsize := SetControlFont(cValueList,APage,IDX);
   DX := ML + cValueList.Left + 2;
   DY := MT + cValueList.Top + fsize + 2;

   {write column title if there is one}
   if cValueList.TitleCaptions.Count > 0 then
      begin
      APage.WriteText(DX,DY,cValueList.TitleCaptions[0] + ' = ' + cValueList.TitleCaptions[1]);
      DY := DY + fSize;
      SI := 1;
      end;

   {write key value pairs}
   for I:=SI to cValueList.RowCount - 1 do
      begin
      AKey := cValueList.Cells[0,I];
      AValue := cValueList.Cells[1,I];
      APage.WriteText(DX,DY,Akey + ' = ' + AValue);
      DY := DY + fSize;
      if (DY > APage.Paper.H - MT) or ((DY > MT + cValueList.Top + cValueList.Height) and fp) then
         begin
         DrawVarBorder(cValueList,APage,DX,DY,ML,MT);
         APage := SetupPage(cValueList,FDoc,ML,MT);
         fsize := SetControlFont(cValueList,APage,IDX);
         DY := MT + cValueList.Top + fsize + 2;
         fp := false;
         end;
      end;

   DrawVarBorder(cValueList,APage,DX,DY,ML,MT);
   end;
end;


{TCheckBox}
procedure CheckBoxToPDF(cCheckBx:TCheckBox; APage:TPDFPage; IDX,ML,MT:integer);
var fSize,                     {font size}
    DX,DY,                     {x and y pos to draw item}
    DW,DH      :integer;       {height and width to draw item}
begin
if cCheckBx.Visible then
   begin
   {write caption}
   fsize := SetControlFont(cCheckBx,APage,IDX);
   DH := cCheckbx.Height;
   DX := ML + cCheckBx.Left + Round(fSize*1.2);       {shift text right to allow for circle}
   DY := MT + cCheckBx.Top + (DH + fsize) div 2;
   APage.WriteText(DX,DY,cCheckbx.Caption);

   {draw tick box}
   DX := ML + cCheckbx.Left;
   DY := DY + 1;
   DW := fSize;                                {want a square box same size as text}
   APage.DrawRect(DX,DY,DW,DW,1,false,true);

   {draw cross}
   if cCheckBx.Checked then
      begin
      DX := DX + 2;
      DY := DY - 2;
      DW := DX + fSize - 4;
      DH := DY - fSize + 4;
      APage.DrawLine(DX,DY,DW,DH,1,true);
      APage.DrawLine(DX,DH,DW,DY,1,true);
      end;
   end;
end;


{TRadioButton}
procedure RadioButToPDF(cRadioBtn:TRadioButton; APage:TPDFPage; IDX,ML,MT:integer);
var fSize,                     {font size}
    DX,DY,                     {x and y pos to draw item}
    DW,DH      :integer;       {height and width to draw item}
begin
if cRadioBtn.Visible then
   begin
   {write caption}
   fsize := SetControlFont(cRadioBtn,APage,IDX);
   DH := cRadioBtn.Height;
   DX := ML + cRadioBtn.Left + Round(fSize*1.2);       {shift text right to allow for circle}
   DY := MT + cRadioBtn.Top + (DH + fsize) div 2;
   APage.WriteText(DX,DY,cRadioBtn.Caption);

   {draw outer circle}
   DX := ML + cRadioBtn.Left;
   DY := DY + 1;
   DW := fsize;                                 {same size as text}
   APage.DrawEllipse(DX,DY,DW,DW,1,false,true);

   {draw inner circle}
   if cRadioBtn.Checked then
      begin
      DX := DX + 4;
      DY := DY - 4;
      DW := DW - 8;
      APage.DrawEllipse(DX,DY,DW,DW,1,true,true);
      end;
   end;
end;


{TRadioGroup}
procedure RadioGroupToPDF(cRadioGrp:TRadioGroup; FDoc:TPDFDocument; APage:TPDFPage; IDX,ML,MT:integer);
var I,
    fSize,                     {font size}
    DX,DY      :integer;       {x and y pos to draw item}
begin
if cRadioGrp.Visible then
   begin
   DrawFixedBorder(cRadioGrp,APage,ML,MT);

   {write caption}
   fsize := SetControlFont(cRadioGrp,APage,IDX);
   DX := ML + cRadioGrp.Left + 2;
   DY := MT + cRadioGrp.Top + fSize + 2;
   APage.WriteText(DX,DY,cRadioGrp.Caption);

   {draw components}
   ML := ML + cRadioGrp.Left;
   MT := MT + cRadioGrp.Top + cRadioGrp.Height - cRadioGrp.ClientHeight;
   for I:=0 to cRadioGrp.ControlCount - 1 do
      ParseControls(cRadioGrp.Controls[I],FDoc,APage,IDX,ML,MT);
   end;
end;


{TCheckGroup}
procedure CheckGroupToPDF(cCheckGrp:TCheckGroup; FDoc:TPDFDocument; APage:TPDFPage; IDX,ML,MT:integer);
var I,
    fSize,                     {font size}
    DX,DY      :integer;       {x and y pos to draw item}
begin
if cCheckGrp.Visible then
   begin
   DrawFixedBorder(cCheckGrp,APage,ML,MT);

   {write caption}
   fsize := SetControlFont(cCheckGrp,APage,IDX);
   DX := ML + cCheckGrp.Left + 2;
   DY := MT + cCheckGrp.Top + fSize + 2;
   APage.WriteText(DX,DY,cCheckGrp.Caption);

   {draw components}
   ML := ML + cCheckGrp.Left;
   MT := MT + cCheckGrp.Top + cCheckGrp.Height - cCheckGrp.ClientHeight;
   for I:=0 to cCheckGrp.ControlCount - 1 do
      ParseControls(cCheckGrp.Controls[I],FDoc,APage,IDX,ML,MT);
   end;
end;


procedure TabSheetToPDF(cTabSht:TTabSheet; FDoc:TPDFDocument; APage:TPDFPage; IDX,ML,MT:integer);
var I          :integer;
begin
if cTabSht.TabVisible then
   begin
   if not FirstPage then APage := SetupPage(cTabSht,FDoc,ML,MT)
      else FirstPage := false;
   for I:=0 to cTabSht.ControlCount - 1 do
      ParseControls(cTabSht.Controls[I],FDoc,APage,IDX,ML,MT);
   end;
end;


{------------------------------------------------------------------------------
Form parsing functions
-------------------------------------------------------------------------------}

procedure ParseControls(AControl:TControl; FDoc:TPDFDocument; Page:TPDFPage; ftText,ML,MT:integer);
{List of simple controls to take action on}
begin
if AControl is TLabel then                {TLabel}
   LabelToPDF(TLabel(AControl),Page,ftText,ML,MT);

if AControl is TStaticText then           {TStaticText}
   StaticTextToPDF(TStaticText(AControl),Page,ftText,ML,MT);

if AControl is TEdit then                 {TEdit}
   EditToPDF(TEdit(AControl),Page,ftText,ML,MT);

if AControl is TFloatSpinEdit then        {TFloatSpinEdit}
   FloatSpinEditToPDF(TFloatSpinEdit(AControl),Page,ftText,ML,MT);

if AControl is TSpinEdit then             {TSpinEdit}
   SpinEditToPDF(TSpinEdit(AControl),Page,ftText,ML,MT);

if AControl is TSpinEditEx then           {TSpinEditEx}
   SpinEditExToPDF(TSpinEditEx(AControl),Page,ftText,ML,MT);

if AControl is TFloatSpinEditEx then      {TFloatSpinEditEx}
   FloatSpinEditExToPDF(TFloatSpinEditEx(AControl),Page,ftText,ML,MT);

if AControl is TDirectoryEdit then        {TDirectoryEdit}
   DirEditToPDF(TDirectoryEdit(AControl),Page,ftText,ML,MT);

if AControl is TFileNameEdit then         {TFileNameEdit}
   FileEditToPDF(TFileNameEdit(AControl),Page,ftText,ML,MT);

if AControl is TComboBox then             {TComboBox}
   ComboBoxToPDF(TComboBox(AControl),Page,ftText,ML,MT);

if AControl is TListBox then              {TListBox}
   ListBoxToPDF(TListBox(AControl),FDoc,Page,ftText,ML,MT);

if AControl is TStringGrid then           {TStringGrid}
   StringGridToPDF(TStringGrid(AControl),FDoc,Page,ftText,ML,MT);

if AControl is TValueListEditor then      {TValueListEditor}
   ValueListToPDF(TValueListEditor(AControl),FDoc,Page,ftText,ML,MT);

if AControl is TMemo then                 {TMemo}
   MemoToPDF(TMemo(AControl),FDoc,Page,ftText,ML,MT);

if AControl is TCheckBox then             {TCheckBox}
   CheckBoxToPDF(TCheckBox(AControl),Page,ftText,ML,MT);

if AControl is TRadioButton then          {TRadioButton}
   RadioButToPDF(TRadioButton(AControl),Page,ftText,ML,MT);

if AControl is TImage then                {TImage}
   ImageToPDF(TImage(AControl),FDoc,Page,ML,MT);

if AControl is TChart then                {TChart}
   ChartToPDF(TChart(AControl),FDoc,Page,ML,MT);

if AControl is TShape then                {TShape}
   ShapeToPDF(TShape(AControl),Page,ML,MT);

if AControl is TPageControl then          {TPageControl}
   RecurseControls(AControl,FDoc,Page,ftText,ML,MT);

if AControl is TTabSheet then             {TTabSheet}
   RecurseControls(AControl,FDoc,Page,ftText,ML,MT);

if AControl is TGroupBox then             {TGroupBox}
   RecurseControls(AControl,FDoc,Page,ftText,ML,MT);

if AControl is TPanel then                {TPanel}
   RecurseControls(AControl,FDoc,Page,ftText,ML,MT);

if AControl is TRadioGroup then           {TRadioGroup}
   RecurseControls(AControl,FDoc,Page,ftText,ML,MT);

if AControl is TCheckGroup then           {TCheckGroup}
   RecurseControls(AControl,FDoc,Page,ftText,ML,MT);
end;


procedure RecurseControls(AControl:TControl; FDoc:TPDFDocument; Page:TPDFPage; ftText,ML,MT:integer);
{Iterate through components and print them to PDF, recurse into nested controls.
Terrible programming but use exit to emulate caes and increase efficiency}
var cForm      :TForm;
    cPageCtrl  :TPageControl;
    I          :integer;
begin
if AControl is TForm then                 {TForm}
   begin
   cForm := AControl as TForm;
   {write page titel centered}
   Header2PDF(cForm,Page,ftText,ML,MT);
   for I:=0 to cForm.ControlCount - 1 do
      ParseControls(cForm.Controls[I],FDoc,Page,ftText,ML,MT);
   exit;
   end;
if AControl is TPageControl then          {TPageControl}
   begin
   cPageCtrl := AControl as TPageControl;
   for I:=0 to cPageCtrl.ControlCount - 1 do
      ParseControls(cPageCtrl.Controls[I],FDoc,Page,ftText,ML,MT);
   exit;
   end;
if AControl is TTabSheet then             {TTabSheet}
   begin
   TabSheetToPDF(TTabSheet(AControl),FDoc,Page,ftText,ML,MT);
   exit;
   end;
if AControl is TGroupBox then             {TGroupBox}
   begin
   GroupBoxToPDF(TGroupBox(AControl),FDoc,Page,ftText,ML,MT);
   exit;
   end;
if AControl is TPanel then                {TPanel}
   begin
   PanelToPDF(TPanel(AControl),FDoc,Page,ftText,ML,MT);
   exit;
   end;
if AControl is TRadioGroup then           {TRadioGroup}
   begin
   RadioGroupToPDF(TRadioGroup(AControl),FDoc,Page,ftText,ML,MT);
   exit;
   end;
if AControl is TCheckGroup then           {TCheckGroup}
   begin
   CheckGroupToPDF(TCheckGroup(AControl),FDoc,Page,ftText,ML,MT);
   exit;
   end;
end;


function FormToPDF:integer;
{Use to check if FormToPDF is available. Resets FDoc.}
begin
FreeAndNil(FDoc);
Result := FormToPDF(nil,'');
end;


function FormToPDF(AControl: TControl):integer;
{Use to append pages to FDoc}
begin
Result := FormToPDF(AControl,'');
end;


function FormToPDF(FileName:string):integer;
{Use to save FDoc to PDF and reset FDoc}
begin
Result := FormToPDF(nil,FileName);
end;


function FormToPDF(AControl: TControl; FileName:string):integer;
{Note screen origin is top-left, pdf origin is bottom-left. We map the form to
the page 1:1, but PDF is 72 dpi (Points) and screen is usually 96dpi so the
form will be enlarged. Returns number of objects printed if successful, error
code otherwise. Error codes:
 0 initialisation OK, no objects printed, nil control or empty filename.
-1 no fonts available.
-2 could not create document}

var DW,DH,
    MT,                        {margin top}
    ML,                        {margin left}
    ftTitle,                   {title font index}
    ftText     :integer;       {text font index}
    Page       :TPDFPage;
    Section    :TPDFSection;
    Aspect     :single;        {control aspect ratio}

begin
Result := -1;
FirstPage := false;

{Checks}
if FontsAvailable then Result := 0;
if (Result = 0) then
   begin
   {set margins}
   MT := 108;                       {1.5 inch}
   ML := 72;                       {1 inch}

   if not Assigned(FDoc) then
      begin
      {Set up document}
      try
         FDoc := TPDFDocument.Create(Nil);
      except
         Result := -2;
      end;
      FDoc.Options := [poPageOriginAtTop];
      FDoc.FontDirectory := 'fonts';
      FDoc.DefaultUnitOfMeasure := uomPixels;
      FDoc.StartDocument;
      Section := FDoc.Sections.AddSection;
      end;

   {set up fonts}
   {It is very difficult to get the system fonts. For now the user must copy
   any fonts used in the form to the /fonts directory and specify them explicitly}
   ftTitle := FDoc.Addfont('Helvetica');
   ftText := FDoc.Addfont('FreeSans.ttf','Regular');

   {if user has not already set info then set defaults}
   if FDoc.Infos.Title <> '' then FDoc.Infos.Title := Application.Title;
   if FDoc.Infos.Author <> '' then FDoc.Infos.Author := 'Form2PDF';
   if FDoc.Infos.Producer <> '' then FDoc.Infos.Producer := 'fpGUI Toolkit 1.4.1';
   if FDoc.Infos.ApplicationName <> '' then FDoc.Infos.ApplicationName := ApplicationName;
   FDoc.Infos.CreationDate := Now;

   if Assigned(AControl) then
      begin
      {get form aspect ratio}
      DW := AControl.Width;
      DH := AControl.Height;
      Aspect := DW/DH;
      if Aspect > 1 then
         FDoc.DefaultOrientation := ppoLandscape
        else
         FDoc.DefaultOrientation := ppoPortrait;

      {set paper size, smaller than A4 use A4 otherwise use custom as nothing larger}
      CustomPaper.H := DH + 2*MT;
      CustomPaper.W := DW + 2*ML;
      CustomPaper.Printable.T := 10;
      CustomPaper.Printable.L := 10;
      CustomPaper.Printable.R := CustomPaper.W - 10;
      CustomPaper.Printable.B := CustomPaper.H - 10;
      if (DW > 842 - 2*ML) or (DH > 595 - 2*MT) then
         FDoc.DefaultPaperType := ptCustom
        else FDoc.DefaultPaperType := ptA4;

      {Add first page}
      Page := SetupPage(AControl,FDoc,ML,MT);
      FirstPage := true;

      RecurseControls(AControl,FDoc,Page,ftText,ML,MT);
      end;

   {Save the PDF}
   if FileName <> '' then
      begin
      FDoc.SaveToFile(FileName);
      Result := FDoc.ObjectCount;
      FreeAndNil(FDoc);         {assume document is finished and dispose}
      end;
   end;
end;


initialization
FDoc := nil;
{add any extra fonts}
gTTFontCache.SearchPath.Add(ExtractFilePath(Application.ExeName) + 'fonts');
{gTTFontCache.ReadStandardFonts;}
gTTFontCache.BuildFontCache;

finalization
FreeAndNil(FDoc);

end.

