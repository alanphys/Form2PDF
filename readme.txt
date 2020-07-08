Form2PDF (c) AC Chamberlain Copyright 2020

1) Introduction
This unit renders (very crudely) the text and image components of a form to a PDF using the fcl-pdf package. The object is not to provide a pixel by pixel representation of the form, but to record the text and image information. Multiline controls such as TStringGrid and TMemo are printed out in their entirety. The unit is modularised so new components can be added easily. 

2) Licence
Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.

3)Supported components are:
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

4) Usage
To use copy form2pdf.pas into your source directory and include Form2PDF in your uses clause. Any visual control can be passed as a parent eg. TTabSheet or TForm.

FormToPDF 
initialise FDoc and check if fonts are available

FormToPDF(AControl: TControl)
parse controls and append pages to Fdoc

FormToPDF(FileName:string)
use to save FDoc to PDF and reset FDoc

FormToPDF(AControl:TControl; FileName:string) 
parse controls and save and close Fdoc

5) History
26/6/2020 Initial commit.
3/7/2020  Fix bottom margin pagination.
5/07/2020 (TvS):moved initialization of FormToPDF to initalization part of unit
6/7/2020  changed FormToPDF to function to return error code
          added control and filename checks
8/7/2020  add functionality to append pages to document, FDoc now global}

6) To Do
Implement word wrapping on TMemo (seems to be OK for Windows)
Text alignment on TEdit, TSpinEdit, TFloatSpinEdit, TSpinEditEx and TFloatSpinEditEx.
Add separate header margin
Implement system fonts
Caption for TPanel

                   
