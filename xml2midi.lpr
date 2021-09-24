//
// Lazarus 2.0.0 project
//

program xml2midi;

{$MODE Delphi}

uses
  Forms,
  lcl, Interfaces,
  umidixml_lazarus in 'umidixml_lazarus.pas',
  UMidiDataStream in 'UMidiDataStream.pas',
  UMyMemoryStream in 'UMyMemoryStream.pas',
  UMyMidiStream in 'UMyMidiStream.pas',
  UEventArray in 'UEventArray.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  if ParamCount > 0 then
  begin
    Form1.ConvertFile(ParamStr(1));
  end else
    Application.Run;
end.



