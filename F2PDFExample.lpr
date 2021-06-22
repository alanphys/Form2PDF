program F2PDFExample;
{.DEFINE DEBUG}
{$mode objfpc}{$H+}

uses
   {$IFDEF UNIX}{$IFDEF UseCThreads}
   cthreads,
   {$ENDIF}{$ENDIF}
   Interfaces, // this includes the LCL widgetset
   Forms, tachartlazaruspkg, lazcontrols, datetimectrls,
   f2pdfexunit;

{$R *.res}

begin
   {Set up -gh output for the Leakview package}
   {$IFDEF DEBUG}
   if FileExists('heap.trc') then
      DeleteFile('heap.trc');
   SetHeapTraceOutput('heap.trc');
   {$ENDIF}
   RequireDerivedFormResource:=True;
   Application.Scaled:=True;
   Application.Initialize;
   Application.CreateForm(TF2PDFExForm, F2PDFExForm);
   Application.Run;
end.

