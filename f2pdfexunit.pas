unit f2pdfexunit;

{$mode objfpc}{$H+}

interface

uses
   Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
   ComCtrls, Grids, Menus, Spin, EditBtn, ValEdit, TAGraph, TASources, TASeries,
   SpinEx;

type

   { TF2PDFExForm }

   TF2PDFExForm = class(TForm)
      Chart1: TChart;
      Chart1LineSeries1: TLineSeries;
      CheckBox1: TCheckBox;
      CheckBox2: TCheckBox;
      CheckBox3: TCheckBox;
      CheckBox4: TCheckBox;
      CheckBox5: TCheckBox;
      CheckBox6: TCheckBox;
      CheckBox7: TCheckBox;
      CheckGroup1: TCheckGroup;
      ComboBox1: TComboBox;
      DirectoryEdit1: TDirectoryEdit;
      Edit1: TEdit;
      FileNameEdit1: TFileNameEdit;
      FloatSpinEdit1: TFloatSpinEdit;
      FloatSpinEdit2: TFloatSpinEdit;
      FloatSpinEditEx1: TFloatSpinEditEx;
      FloatSpinEditEx2: TFloatSpinEditEx;
      GroupBox1: TGroupBox;
      Image: TImage;
      Label1: TLabel;
      Label10: TLabel;
      Label11: TLabel;
      Label12: TLabel;
      Label13: TLabel;
      Label14: TLabel;
      Label15: TLabel;
      Label16: TLabel;
      Label17: TLabel;
      Label18: TLabel;
      Label19: TLabel;
      Label2: TLabel;
      Label20: TLabel;
      Label21: TLabel;
      Label3: TLabel;
      Label4: TLabel;
      Label5: TLabel;
      Label6: TLabel;
      Label7: TLabel;
      Label8: TLabel;
      Label9: TLabel;
      ListBox1: TListBox;
      MainMenu: TMainMenu;
      Memo1: TMemo;
      miPrintActive: TMenuItem;
      miPrintAll: TMenuItem;
      PageControl1: TPageControl;
      Panel1: TPanel;
      Panel2: TPanel;
      Panel3: TPanel;
      RadioButton1: TRadioButton;
      RadioButton2: TRadioButton;
      RadioButton3: TRadioButton;
      RadioButton4: TRadioButton;
      RadioButton5: TRadioButton;
      RadioButton6: TRadioButton;
      RadioButton7: TRadioButton;
      RadioButton8: TRadioButton;
      RadioGroup1: TRadioGroup;
      RandomChartSource1: TRandomChartSource;
      SaveDialog: TSaveDialog;
      Shape1: TShape;
      Shape10: TShape;
      Shape11: TShape;
      Shape12: TShape;
      Shape13: TShape;
      Shape14: TShape;
      Shape15: TShape;
      Shape16: TShape;
      Shape2: TShape;
      Shape3: TShape;
      Shape4: TShape;
      Shape5: TShape;
      Shape6: TShape;
      Shape7: TShape;
      Shape8: TShape;
      Shape9: TShape;
      SpinEdit1: TSpinEdit;
      SpinEdit2: TSpinEdit;
      SpinEditEx1: TSpinEditEx;
      SpinEditEx2: TSpinEditEx;
      StaticText1: TStaticText;
      StaticText2: TStaticText;
      StaticText3: TStaticText;
      StaticText4: TStaticText;
      StaticText5: TStaticText;
      StaticText6: TStaticText;
      StringGrid2: TStringGrid;
      StringGrid1: TStringGrid;
      TabSheet1: TTabSheet;
      TabSheet2: TTabSheet;
      TabSheet3: TTabSheet;
      TabSheet4: TTabSheet;
      TabSheet5: TTabSheet;
      TabSheet6: TTabSheet;
      TabSheet7: TTabSheet;
      TabSheet8: TTabSheet;
      TabSheet9: TTabSheet;
      ValueListEditor1: TValueListEditor;
      procedure FormCreate(Sender: TObject);
      procedure miPrintActiveClick(Sender: TObject);
      procedure miPrintAllClick(Sender: TObject);
      procedure PageControl1Change(Sender: TObject);
   private

   public

   end;

var
   F2PDFExForm: TF2PDFExForm;

implementation

uses form2pdf;

{$R *.lfm}

{ TF2PDFExForm }

procedure TF2PDFExForm.FormCreate(Sender: TObject);
var I,J        :integer;
begin
Image.Picture.LoadFromFile('images/lena.bmp');
for I:=0 to StringGrid2.RowCount - 1 do
   for J:=0 to StringGrid2.ColCount - 1 do
      StringGrid2.Cells[J,I] := IntToStr(I+J*J);

StringGrid1.RowCount := 25;
for I:=1 to StringGrid1.RowCount - 1 do
   begin
   StringGrid1.Cells[0,I] := IntToStr(I);
   for J:=1 to StringGrid1.ColCount - 1 do
      StringGrid1.Cells[J,I] := IntToStr(I+J);
   end;

end;

procedure TF2PDFExForm.miPrintActiveClick(Sender: TObject);
begin
Form2PDF.FormToPDF(PageControl1.ActivePage,'test.pdf');
end;

procedure TF2PDFExForm.miPrintAllClick(Sender: TObject);
begin
Form2PDF.FormToPDF(F2PDFExForm,'test.pdf');
end;

procedure TF2PDFExForm.PageControl1Change(Sender: TObject);
begin

end;


end.

