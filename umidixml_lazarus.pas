unit umidixml_lazarus;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Laz2_Dom, Laz2_XMLRead, Laz2_XMLWrite, Variants;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    Memo: TMemo;
    XMLDocument1: TXMLDocument;
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
  public
    procedure ConvertFile(const Filename: string);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  UMyMidiStream, UEventArray;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Label1.Font.Color := clRed;
end;


procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  FileName: string;
begin
  for FileName in FileNames do
  begin
    ConvertFile(FileName);
  end;
end;


procedure TForm1.ConvertFile(const Filename: string);
var
  MidiEvent: TMidiEvent;
  Partitur: TEventArray;
  iTrack, iEvent: integer;

  procedure WriteAttributes(Node: TDomNode);
  var
    q: integer;
    ole: oleVariant;
  begin
    if Node.Attributes.Length > 0 then
    begin
      Memo.Append('    Attibutes and Values');
      for q := 0 to Node.Attributes.Length-1 do
        with Node.Attributes[q] do
          Memo.Append('        ' + NodeName + '    ' + NodeValue);
    end;
    ole := Node.NodeValue;
    if not VarIsNull(ole) then
      Memo.Append('    NodeValue: ' + Node.NodeValue);
  end;

  procedure AppendEvent;
  begin
    inc(iEvent);
    SetLength(Partitur.TrackArr[iTrack], iEvent+1);
    Partitur.TrackArr[iTrack][iEvent] := MidiEvent;
  end;

  function GetNodeValue(Node: TDomNode): string;
  begin
    result := '';
    // Node.NodeValue is not implemented!
    if (Node.FirstChild <> nil) and
       (Node.FirstChild.NodeName = '#text') then
      result := Node.FirstChild.NodeValue;
  end;

  procedure AppendMetaEvent(Nr: integer; Node: TDomNode);
  var
    s: string;
  begin
    MidiEvent.d1 := Nr;
    s := GetNodeValue(Node);
    MidiEvent.AppendString(s);
    if s <> '' then
      AppendEvent;
  end;

  function GetAttribute(Node: TDomNode; Value: string): string;
  var
    Item: TDomNode;
  begin
    result := '';
    Item := Node.Attributes.GetNamedItem(Value);
    if Item <> nil then
      result := GetNodeValue(Item);
  end;

var
  mem: TMemoryStream;
  s: AnsiString;
  t: string;
  Delta: string;
  i, k, n: integer;
  tempo: integer;
  Root, Track, Event, Node1, Node2: TDomNode;
  TicksPerBeat: integer;
  FirstTrack: boolean;
  SaveName: string;
  TimestampType: string;
  iAbsolute, iOffset: integer;
  XMLDocument: TXMLDocument;

  procedure MemoAppend(s: string);
  begin
    Memo.Append('');
    Memo.Append('Path: ' + ExtractFilePath(FileName));
    Memo.Append(s);
    if Memo.VertScrollBar.Visible then
      Memo.VertScrollBar.Position := (Memo.Lines.Count) + Memo.Font.Height;
    writeln(s);
  end;

begin
  if not FileExists(Filename) then
  begin
    MemoAppend(Filename + ' does not exist!');
    exit;
  end;
  if LowerCase(ExtractFileExt(Filename)) = '.mid' then
  begin
    MemoAppend('There is no reverse conversion available!');
    exit;
  end;

  Savename := Filename;
  SetLength(Savename, Length(Savename) - Length(ExtractFileExt(Savename)));
  Savename := Savename + '.mid';
  if FileExists(Savename) then
  begin
    MemoAppend(ExtractFilename(Savename) + ' already exists!');
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
      MemoAppend(ExtractFilename(Filename) + ' is not a "MIDI-XML" File');
      exit;
    end;
    for i := 0 to mem.size-n do
      PByte(mem.Memory)[i] := PByte(mem.Memory)[i+n-1];
    mem.Size := mem.Size-n+1;

    MemoAppend('convert: ' + ExtractFilename(Filename));

    ReadXMLFile(XMLDocument, mem);
    Root := XMLDocument.FindNode('MIDIFile');
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
                tempo := StrToIntDef(GetAttribute(Node1, 'Value'), 500000);
                MidiEvent.AppendByte(tempo shr 16);
                MidiEvent.AppendByte((tempo shr 8) and $ff);
                MidiEvent.AppendByte(tempo and $ff);
                AppendEvent;
              end else
              if t = 'TimeSignature' then
              begin
                MidiEvent.d1 := $59;
                MidiEvent.AppendByte(StrToIntDef(GetAttribute(Node1, 'Numerator'), 4));
                MidiEvent.AppendByte(StrToIntDef(GetAttribute(Node1, 'LogDenominator'), 2));
                MidiEvent.AppendByte(StrToIntDef(GetAttribute(Node1, 'MIDIClocksPerMetronomeClick'), 24));
                MidiEvent.AppendByte(StrToIntDef(GetAttribute(Node1, 'ThirtySecondsPer24Clocks'), 8));
                AppendEvent;
              end else
              if t = 'KeySignature' then
              begin
                MidiEvent.d1 := $58;
                MidiEvent.AppendByte(StrToIntDef(GetAttribute(Node1, 'Fifths'), 4));
                MidiEvent.AppendByte(StrToIntDef(GetAttribute(Node1, 'Mode'), 2));
                AppendEvent;
              end else
              if t = 'EndOfTrack' then
              begin
              end else
              if t = 'MIDIChannelPrefix' then
              begin
                MidiEvent.d1 := $20;
                MidiEvent.AppendByte(StrToIntDef(GetAttribute(Node1, 'Value'), 4));
                AppendEvent;
              end else
              if t = 'ProgramChange' then
              begin
                MidiEvent.command := StrToIntDef(GetAttribute(Node1, 'Channel'), 0) or $C0;
                MidiEvent.d1 := StrToIntDef(GetAttribute(Node1, 'Number'), 0);
                AppendEvent;
              end else
              if t = 'ControlChange' then
              begin
                MidiEvent.command := StrToIntDef(GetAttribute(Node1, 'Channel'), 0) or $B0;
                MidiEvent.d1 := StrToIntDef(GetAttribute(Node1, 'Control'), 0);
                MidiEvent.d1 := StrToIntDef(GetAttribute(Node1, 'Value'), 0);
                AppendEvent;
              end else
              if (t = 'NoteOn') or (t = 'NoteOff') then
              begin
                MidiEvent.command := StrToIntDef(GetAttribute(Node1, 'Channel'), 0);
                MidiEvent.d1 := StrToIntDef(GetAttribute(Node1, 'Note'), 0);
                MidiEvent.d2 := StrToIntDef(GetAttribute(Node1, 'Velocity'), 0);
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
                MemoAppend('event error: "' + t + '" is not implemented yet!');
                WriteAttributes(Node1);
              end;
            end;
          end;
        end;
      end else begin
        MemoAppend('header error: "' + t + '" is not implemented yet!');
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

