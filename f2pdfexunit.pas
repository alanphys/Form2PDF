unit f2pdfexunit;

{$mode objfpc}{$H+}

interface

uses
   Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
   ComCtrls, Grids, Menus, Spin, EditBtn, ValEdit, TAGraph, TASources, TASeries,
   SpinEx, DateTimePicker;

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
      DateTimePicker1: TDateTimePicker;
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
      Label22: TLabel;
      Label23: TLabel;
      Label24: TLabel;
      Label25: TLabel;
      Label26: TLabel;
      Label27: TLabel;
      Label28: TLabel;
      Label29: TLabel;
      Label3: TLabel;
      Label30: TLabel;
      Label31: TLabel;
      Label32: TLabel;
      Label33: TLabel;
      Label34: TLabel;
      Label35: TLabel;
      Label36: TLabel;
      Label37: TLabel;
      Label38: TLabel;
      Label39: TLabel;
      Label4: TLabel;
      Label40: TLabel;
      Label41: TLabel;
      Label42: TLabel;
      Label43: TLabel;
      Label44: TLabel;
      Label45: TLabel;
      Label46: TLabel;
      Label47: TLabel;
      Label48: TLabel;
      Label49: TLabel;
      Label5: TLabel;
      Label50: TLabel;
      Label51: TLabel;
      Label52: TLabel;
      Label53: TLabel;
      Label54: TLabel;
      Label55: TLabel;
      Label56: TLabel;
      Label57: TLabel;
      Label58: TLabel;
      Label59: TLabel;
      Label6: TLabel;
      Label60: TLabel;
      Label61: TLabel;
      Label62: TLabel;
      Label63: TLabel;
      Label64: TLabel;
      Label65: TLabel;
      Label66: TLabel;
      Label67: TLabel;
      Label68: TLabel;
      Label69: TLabel;
      Label7: TLabel;
      Label70: TLabel;
      Label8: TLabel;
      Label9: TLabel;
      ListBox1: TListBox;
      MainMenu: TMainMenu;
      Memo1: TMemo;
      miPrintSel: TMenuItem;
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
      StatusBar: TStatusBar;
      StringGrid2: TStringGrid;
      StringGrid1: TStringGrid;
      TabSheet1: TTabSheet;
      TabSheet10: TTabSheet;
      TabSheet11: TTabSheet;
      TabSheet12: TTabSheet;
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
      procedure miPrintSelClick(Sender: TObject);
   private

   public

   end;

var
   F2PDFExForm: TF2PDFExForm;

implementation

uses form2pdf, StrUtils;

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
StatusBar.SimpleText := IntToStr(FormToPDF(PageControl1.ActivePage,'test.pdf'))
   + ' objects printed'
end;

procedure TF2PDFExForm.miPrintAllClick(Sender: TObject);
begin
if FormToPDF = 0 then
   StatusBar.SimpleText := IntToStr(FormToPDF(F2PDFExForm,'test.pdf')) + ' objects printed'
  else
   StatusBar.SimpleText := 'Sorry, no fonts available' ;
end;


procedure TF2PDFExForm.miPrintSelClick(Sender: TObject);
var sTabs      :string;
    ObjectCount,
    TabIndex   :integer;
begin
sTabs := '1,2,3,4,5,6,7,8,9';
ObjectCount := 0;
if InputQuery('Please select tabs to be printed','Enter the tab numbers delimited by commas',
   sTabs) then
   try
   while sTabs <> '' do
      begin
      TabIndex := StrToInt(Copy2SymbDel(sTabs,',')) - 1;
      ObjectCount := ObjectCount + FormToPDF(PageControl1.Pages[TabIndex]);
      end;
   StatusBar.SimpleText := IntToStr(FormToPDF('test.pdf')) + ' objects printed'
   except
      StatusBar.SimpleText := 'Sorry, not a valid tab list';
      end;
end;


end.

