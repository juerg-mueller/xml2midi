unit UMidiXml;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Xml.xmldom, Xml.XMLIntf, Xml.XMLDoc,
  Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    XMLDocument1: TXMLDocument;
    Label1: TLabel;
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    procedure ConvertFile(const Filename: string);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  ShellApi, UMyMidiStream, UEventArray;

procedure TForm1.FormCreate(Sender: TObject);
begin
  DragAcceptFiles(Self.Handle, true);
end;

procedure TForm1.WMDropFiles(var Msg: TWMDropFiles);
var
  DropH: HDROP;               // drop handle
  DroppedFileCount: Integer;  // number of files dropped
  FileNameLength: Integer;    // length of a dropped file name
  FileName: string;           // a dropped file name
  i: integer;
begin
  inherited;

  DropH := Msg.Drop;
  try
    DroppedFileCount := DragQueryFile(DropH, $FFFFFFFF, nil, 0);
    if (DroppedFileCount > 0) then
    begin
      for i := 0 to DroppedFileCount-1 do
      begin
        FileNameLength := DragQueryFile(DropH, i, nil, 0);
        SetLength(FileName, FileNameLength);
        DragQueryFile(DropH, i, PChar(FileName), FileNameLength + 1);
//        if LowerCase(ExtractFileExt(Filename)) = '.xml' then
//        begin
          ConvertFile(FileName);
//        end else
//          writeln(Filename, ' ist not a ".xml" file');
      end;
    end;
  finally
    DragFinish(DropH);
  end;
  Msg.Result := 0;
end;

procedure TForm1.ConvertFile(const Filename: string);
var
  MidiEvent: TMidiEvent;
  Partitur: TEventArray;
  iTrack, iEvent: integer;

  procedure WriteAttributes(Node: IXMLNode);
  var
    q: integer;
    ole: oleVariant;
  begin
    if Node.AttributeNodes.Count > 0 then
    begin
      writeln('    Attibutes and Values');
      for q := 0 to Node.AttributeNodes.Count-1 do
        with Node.AttributeNodes[q] do
          writeln('        ', NodeName, '    ', NodeValue);
    end;
    ole := Node.NodeValue;
    if not VarIsNull(ole) then
      writeln('    NodeValue: ', Node.NodeValue);
  end;

  procedure AppendEvent;
  begin
    inc(iEvent);
    SetLength(Partitur.TrackArr[iTrack], iEvent+1);
    Partitur.TrackArr[iTrack][iEvent] := MidiEvent;
  end;

  function GetNodeValue(Node: IXMLNode): string;
  var
    ole: oleVariant;
  begin
    result := '';
    ole := Node.NodeValue;
    if VarIsNull(ole) then
//      writeln('NodeValue is null ', Node.NodeName)
    else
      result := Node.NodeValue;
  end;

  procedure AppendMetaEvent(Nr: integer; Node: IXMLNode);
  var
    s: string;
  begin
    MidiEvent.d1 := Nr;
    s := GetNodeValue(Node);
    MidiEvent.AppendString(s);
    if s <> '' then
      AppendEvent;
  end;

var
  mem: TMemoryStream;
  s: AnsiString;
  t: string;
  Delta: string;
  i, k, n: integer;
  tempo: integer;
  Root, Track, Event, Node1, Node2: IXMLNode;
  TicksPerBeat: integer;
  FirstTrack: boolean;
  SaveName: string;
  TimestampType: string;
  iAbsolute, iOffset: integer;
begin
  Savename := Filename;
  SetLength(Savename, Length(Savename) - Length(ExtractFileExt(Savename)));
  Savename := Savename + '.mid';
  if FileExists(Savename) then
  begin
    writeln(Savename, ' already exists!');
    exit;
  end;

  Partitur := TEventArray.Create;
  mem := TMemoryStream.Create;
  try
    mem.LoadFromFile(Filename);
    SetLength(s, mem.Size);
    for i := 0 to mem.Size-1 do
      s[i+1] := PAnsiChar(mem.Memory)[i];
    n := Pos(AnsiString('<MIDIFile>'), s);
    if n <= 0 then
    begin
      writeln(Filename, ' is not a "MIDI-XML" File');
      exit;
    end;

    writeln('convert ', Filename);

    System.Delete(s, 1, n-1);
    XMLDocument1.LoadFromXML(s);
    Root := XMLDocument1.Node.ChildNodes[0]; // MIDIFile
    t := Root.NodeName;
    FirstTrack := true;
    TicksPerBeat := 120;
    TimestampType := 'Delta';
    iTrack := -1;
    for i := 0 to Root.ChildNodes.Count-1 do
    begin
      Track := Root.ChildNodes[i];
      t := Track.NodeName;
      if t = 'Format' then
      else
      if t = 'TrackCount' then
      else
      if t = 'TicksPerBeat' then
      begin
        Partitur.DetailHeader.DeltaTimeTicks := StrToIntDef(GetNodeValue(Track), 120);
      end else
      if t = 'TimestampType' then
        TimestampType := GetNodeValue(Track) // Absolute or Delta
      else
      if t = 'Track' then
      begin
        inc(iTrack);
        SetLength(Partitur.TrackArr, iTrack+1);
        MidiEvent.Clear;
        iEvent := -1;
        iOffset := 0;
        AppendEvent;
        for k := 0 to Track.ChildNodes.Count-1 do
        begin
          Event := Track.ChildNodes[k];
          Delta := '0';
          if (Event.NodeName = 'Event') then
          begin
            MidiEvent.Clear;
            MidiEvent.command := $ff;
            for n := 0 to Event.ChildNodes.Count-1 do
            begin
              Node1 := Event.ChildNodes[n];
              t := Node1.NodeName;
              if t = 'Delta' then
                inc(Partitur.TrackArr[iTrack][iEvent].var_len, StrToIntDef(GetNodeValue(Node1), 0))
              else
              if t = 'Absolute' then
              begin
                iAbsolute := StrToIntDef(GetNodeValue(Node1), 0);
                if iAbsolute > iOffset then
                begin
                  inc(Partitur.TrackArr[iTrack][iEvent].var_len, iAbsolute - iOffset);
                  iOffset := iAbsolute;
                end;
              end else
              if t = 'TextEvent' then
              begin
                AppendMetaEvent(1, Node1);
              end else
              if t = 'CopyrightNotice' then
              begin
                AppendMetaEvent(2, Node1);
              end else
              if t = 'TrackName' then
              begin
                AppendMetaEvent(3, Node1);
              end else
              if t = 'InstrumentName' then
              begin
                AppendMetaEvent(4, Node1);
              end else
              if t = 'Lyric' then
              begin
                AppendMetaEvent(5, Node1);
              end else
              if t = 'Marker' then
              begin
                AppendMetaEvent(6, Node1);
              end else
              if t = 'CuePoint' then
              begin
                AppendMetaEvent(7, Node1);
              end else
              if t = 'SetTempo' then
              begin
                MidiEvent.d1 := $51;
                tempo := StrToIntDef(Node1.Attributes['Value'], 500000);
                MidiEvent.AppendByte(tempo shr 16);
                MidiEvent.AppendByte((tempo shr 8) and $ff);
                MidiEvent.AppendByte(tempo and $ff);
                AppendEvent;
              end else
              if t = 'TimeSignature' then
              begin
                MidiEvent.d1 := $58;
                MidiEvent.AppendByte(StrToIntDef(Node1.Attributes['Numerator'], 4));
                MidiEvent.AppendByte(StrToIntDef(Node1.Attributes['LogDenominator'], 2));
                MidiEvent.AppendByte(StrToIntDef(Node1.Attributes['MIDIClocksPerMetronomeClick'], 24));
                MidiEvent.AppendByte(StrToIntDef(Node1.Attributes['ThirtySecondsPer24Clocks'], 8));
                AppendEvent;
              end else
              if t = 'KeySignature' then
              begin
                MidiEvent.d1 := $59;
                MidiEvent.AppendByte(StrToIntDef(Node1.Attributes['Fifths'], 4));
                MidiEvent.AppendByte(StrToIntDef(Node1.Attributes['Mode'], 2));
                AppendEvent;
              end else
              if t = 'EndOfTrack' then
              begin
//                Stream.AppendTrackEnd(false);
              end else
              if t = 'MIDIChannelPrefix' then
              begin
                MidiEvent.d1 := $20;
                MidiEvent.AppendByte(StrToIntDef(Node1.Attributes['Value'], 4));
                AppendEvent;
              end else
              if t = 'ProgramChange' then
              begin
                MidiEvent.command := StrToIntDef(Node1.Attributes['Channel'], 0) or $C0;
                MidiEvent.d1 := StrToIntDef(Node1.Attributes['Number'], 0);
                AppendEvent;
              end else
              if t = 'ControlChange' then
              begin
                MidiEvent.command := StrToIntDef(Node1.Attributes['Channel'], 0) or $B0;
                MidiEvent.d1 := StrToIntDef(Node1.Attributes['Control'], 0);
                MidiEvent.d2 := StrToIntDef(Node1.Attributes['Value'], 0);
                AppendEvent;
              end else
              if (t = 'NoteOn') or (t = 'NoteOff') then
              begin
                MidiEvent.command := StrToInt(Node1.Attributes['Channel']);
                MidiEvent.d1 := StrToInt(Node1.Attributes['Note']);
                MidiEvent.d2 := StrToInt(Node1.Attributes['Velocity']);
                if (t = 'NoteOn') and (MidiEvent.d2 = 0) then
                  t := 'NoteOff';
                if t = 'NoteOn' then
                  inc(MidiEvent.command, $90)
                else begin
                  inc(MidiEvent.command, $80);
                  MidiEvent.d2 := $40;
                end;
                AppendEvent;
              end else begin
                writeln('event error: "', t, '" is not implemented yet!');
                WriteAttributes(Node1);
              end;
            end;
          end;
        end;
      end else begin
        writeln('header error: "', t, '" is not implemented yet!');
        WriteAttributes(Track);
      end;
    end;
    Partitur.SaveMidiToFile(Savename, false);
  finally
    mem.Free;
    Partitur.Free;
  end;
end;

end.
