program F2PDFExample;
{.DEFINE DEBUG}
{$mode objfpc}{$H+}

uses
   {$IFDEF UNIX}{$IFDEF UseCThreads}
   cthreads,
   {$ENDIF}{$ENDIF}
   Interfaces, // this includes the LCL widgetset
   Forms, tachartlazaruspkg, lazcontrols, printer4lazarus, f2pdfexunit
   {$IFDEF DEBUG}
   , SysUtils              //delete SysUtils if not using heaptrc
   {$ENDIF}
   { you can add units after this };

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

