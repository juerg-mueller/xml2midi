program xml2midi;

uses
  Vcl.Forms,
  UMidiXml in 'UMidiXml.pas' {Form1},
  UMidiDataStream in 'UMidiDataStream.pas',
  UMyMemoryStream in 'UMyMemoryStream.pas',
  UMyMidiStream in 'UMyMidiStream.pas',
  UEventArray in 'UEventArray.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
