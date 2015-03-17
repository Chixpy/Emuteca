unit uCHXIni;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, LazUTF8;

type

  { cCHXIniLine }

  cCHXIniLine = class(TObject)
  private
    FComment: string;
    FKey: string;
    FValue: string;
    procedure SetComment(AValue: string);
    procedure SetKey(AValue: string);
    procedure SetValue(AValue: string);

  protected

  public
    property Key: string read FKey write SetKey;
    property Value: string read FValue write SetValue;
    property Comment: string read FComment write SetComment;

    constructor Create(const aKey, aValue, aComment: string);
    destructor Destroy; override;
  end;

  cCHXIniLinesList = specialize TFPGObjectList<cCHXIniLine>;

  { cCHXIniSection }

  cCHXIniSection = class(TObject)
  private
    FComment: string;
    FLines: cCHXIniLinesList;
    FName: string;
    procedure SetComment(AValue: string);
    procedure SetLines(AValue: cCHXIniLinesList);
    procedure SetName(AValue: string);

  protected

  public
    property Name: string read FName write SetName;
    property Comment: string read FComment write SetComment;
    {< Comment in the section line. }

    property Lines: cCHXIniLinesList read FLines write SetLines;

    function LineByKey(aKey: string): cCHXIniLine;
    function ValueByKey(aKey: string): string;
    function AddLine(aKey, aValue, aComment: string; MergeKeys: Boolean): cCHXIniLine;

    constructor Create(const aName, aComment: string);
    destructor Destroy; override;
  end;

  { cCHXIniSectionList }
  cCHXIniSectionList = specialize TFPGObjectList<cCHXIniSection>;

  { cCHXIni }

  cCHXIni = class(TObject)
  private
    FAssignChar: char;
    FCommentBegin: char;
    FEscapeLF: boolean;
    FEscapeLFChar: char;
    FEscapeLFMaxWidth: word;
    FFileName: string;
    FMergeKeys: boolean;
    FMergeSections: boolean;
    FNewLine: string;
    FQuoteBegin: char;
    FQuoteEnd: char;
    FRemoveQuotes: Boolean;
    FSectionBegin: char;
    FSectionEnd: char;
    FSectionList: cCHXIniSectionList;
    procedure SetAssignChar(AValue: char);
    procedure SetCommentBegin(AValue: char);
    procedure SetEscapeLF(AValue: boolean);
    procedure SetEscapeLFChar(AValue: char);
    procedure SetEscapeLFMaxWidth(AValue: word);
    procedure SetFileName(AValue: string);
    procedure SetMergeKeys(AValue: boolean);
    procedure SetMergeSections(AValue: boolean);
    procedure SetNewLine(AValue: string);
    procedure SetQuoteBegin(AValue: char);
    procedure SetQuoteEnd(AValue: char);
    procedure SetRemoveQuotes(AValue: Boolean);
    procedure SetSectionBegin(AValue: char);
    procedure SetSectionEnd(AValue: char);

  protected
    property SectionList: cCHXIniSectionList read FSectionList;

    procedure FillSectionList(aStringList: TStrings);

  public
    property FileName: string read FFileName write SetFileName;
    property EscapeLF: boolean read FEscapeLF write SetEscapeLF;
    property MergeSections: boolean read FMergeSections write SetMergeSections;
    property MergeKeys: boolean read FMergeKeys write SetMergeKeys;

    property CommentBegin: char read FCommentBegin write SetCommentBegin;
    property SectionBegin: char read FSectionBegin write SetSectionBegin;
    property SectionEnd: char read FSectionEnd write SetSectionEnd;
    property NewLine: string read FNewLine write SetNewLine;
    property RemoveQuotes: Boolean read FRemoveQuotes write SetRemoveQuotes;
    {< Quotes are removed when a Value is readed, internally
      the value is stored as is.
    }
    property QuoteBegin: char read FQuoteBegin write SetQuoteBegin;
    property QuoteEnd: char read FQuoteEnd write SetQuoteEnd;
    property AssignChar: char read FAssignChar write SetAssignChar;
    property EscapeLFChar: char read FEscapeLFChar write SetEscapeLFChar;
    property EscapeLFMaxWidth: word read FEscapeLFMaxWidth
      write SetEscapeLFMaxWidth;

    procedure LoadFromFile(const aFilename: string);
    {< Reads a ini file from disk.
    }
    procedure SaveToFile(const aFilename: string);
    {< Save a ini file to disk (it doesn't merge content).

      For merging, target file must be loaded first.
    }
    function SectionNameList: TStringList;
    {< Creates a TStringlist with the name of setions.
    }
    function SectionByIndex(aIndex: integer): cCHXIniSection;
    {< Returns the section at aIndex position in the Ini file.

    Section 0 is the text encountered before any [Section] heading.

    This way you actually can access to any section in the file.
    }
    function SectionByName(aSectionName: string): cCHXIniSection;
    {< Returns the section with aSectionName name.

    If many sections have the same name, the first one is returned. So you can't
      access to all sections this way.

    The text encountered before any section is in the first "virtual" section,
      under the name '' (empty string).
    }
    function SectionCount: integer;
    {< Returns the number of sections in the file.

    It includes the text before any section ("virtual" Section 0 or ''), even
      if it's empty.
    }

    // Main reading string functions
    function ReadString(const aSectionName, aKeyName, aDefault: string): string;
    {< Accesses a value by section and key name. }
    function ReadString(const aSectionIndex: integer; const aKeyName,
      aDefault: string): string;
    {< Accesses a value by section index and key name. }
    function ReadString(const aSectionIndex, aKeyIndex: integer; const
      aDefault: string): string;
    {< Accesses a value by section index and line. }


    function ReadMultiStrings(const aSectionName, aKeyName, aDefault: string; const Separator: char = ','): TStringList;
    function ReadInteger(const aSectionName, aKeyName: string; const aDefault: integer): integer;
    function ReadTPoint(const aSectionName, aKeyName: string; const aDefault: TPoint): TPoint;

    // Common ini writing functions
    procedure WriteString(const aSectionName, aKeyName, aValue: string);
    procedure WriteInteger(const aSectionName, aKeyName: string; const aValue: integer);

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ cCHXIniLine }

procedure cCHXIniLine.SetComment(AValue: string);
begin
  FComment := AValue;
end;

procedure cCHXIniLine.SetKey(AValue: string);
begin
  FKey := UTF8Trim(AValue);
end;

procedure cCHXIniLine.SetValue(AValue: string);
begin
  FValue := AValue;
end;

constructor cCHXIniLine.Create(const aKey, aValue, aComment: string);
begin
  Key := UTF8Trim(aKey);
  if Key <> '' then
    Value := UTF8Trim(aValue)
  else
    Value := aValue;
  Comment := aComment;
  // inline comments trimmed
  if (key <> '') or (Value <> '') then
    Comment := UTF8Trim(Comment);
end;

destructor cCHXIniLine.Destroy;
begin
  inherited Destroy;
end;

{ cCHXIniSection }

procedure cCHXIniSection.SetComment(AValue: string);
begin
  FComment := AValue;
end;

procedure cCHXIniSection.SetLines(AValue: cCHXIniLinesList);
begin
  FLines := AValue;
end;

procedure cCHXIniSection.SetName(AValue: string);
begin
  FName := AValue;
end;

function cCHXIniSection.LineByKey(aKey: string): cCHXIniLine;
var
  CurrLine: cCHXIniLine;
  i: integer;
begin
  Result := nil;
  aKey := UTF8Trim(aKey);
  if aKey = '' then
    Exit;

  i := 0;
  while i < Lines.Count do
  begin
    CurrLine := Lines[i];
    if UTF8CompareText(CurrLine.Key, aKey) = 0 then
    begin
      // Found!
      Result := CurrLine;
      break;
    end;
    Inc(i);
  end;
end;

function cCHXIniSection.ValueByKey(aKey: string): string;
var
  aLine: cCHXIniLine;
begin
  Result := '';
  aLine := LineByKey(aKey);
  if aLine<>nil then Exit;
  Result := aLine.Value;
end;

function cCHXIniSection.AddLine(aKey, aValue, aComment: string;
  MergeKeys: Boolean): cCHXIniLine;
begin
  Result := nil;

  aKey := UTF8Trim(aKey);

  if MergeKeys and (aKey <> '') then
    Result := LineByKey(aKey);

  if Result = nil then
  begin
    Result := cCHXIniLine.Create(aKey, aValue, aComment);
    Lines.Add(Result);
  end
  else
  begin
    Result.Value := aValue;
    Result.Comment := Result.Comment + ' / ' +aComment;
  end;
end;

constructor cCHXIniSection.Create(const aName, aComment: string);
begin
  inherited Create;
  self.Name := UTF8Trim(aName);
  self.Comment := UTF8Trim(aComment); //Inline comments trimmed
  FLines := cCHXIniLinesList.Create(True);
end;

destructor cCHXIniSection.Destroy;
begin
  FreeAndNil(FLines);
  inherited Destroy;
end;

{ cCHXIni }

procedure cCHXIni.SetCommentBegin(AValue: char);
begin
  FCommentBegin := AValue;
end;

procedure cCHXIni.SetAssignChar(AValue: char);
begin
  FAssignChar := AValue;
end;

procedure cCHXIni.SetEscapeLF(AValue: boolean);
begin
  FEscapeLF := AValue;
end;

procedure cCHXIni.SetEscapeLFChar(AValue: char);
begin
  FEscapeLFChar := AValue;
end;

procedure cCHXIni.SetEscapeLFMaxWidth(AValue: word);
begin
  FEscapeLFMaxWidth := AValue;
end;

procedure cCHXIni.SetFileName(AValue: string);
begin
  FFileName := AValue;
end;

procedure cCHXIni.SetMergeKeys(AValue: boolean);
begin
  FMergeKeys := AValue;
end;

procedure cCHXIni.SetMergeSections(AValue: boolean);
begin
  FMergeSections := AValue;
end;

procedure cCHXIni.SetNewLine(AValue: string);
begin
  FNewLine := AValue;
end;

procedure cCHXIni.SetQuoteBegin(AValue: char);
begin
  FQuoteBegin := AValue;
end;

procedure cCHXIni.SetQuoteEnd(AValue: char);
begin
  FQuoteEnd := AValue;
end;

procedure cCHXIni.SetRemoveQuotes(AValue: Boolean);
begin
  if FRemoveQuotes = AValue then Exit;
  FRemoveQuotes := AValue;
end;

procedure cCHXIni.SetSectionBegin(AValue: char);
begin
  FSectionBegin := AValue;
end;

procedure cCHXIni.SetSectionEnd(AValue: char);
begin
  FSectionEnd := AValue;
end;

procedure cCHXIni.FillSectionList(aStringList: TStrings);

  procedure ExtractInlineComment(const Str: string; var aValue, aComment: string);
  var
    aPos: integer;
  begin
    aPos := UTF8Pos(CommentBegin, Str);
    if aPos <> 0 then
    begin
      // Has a comment
      aComment := UTF8Trim(UTF8Copy(Str, aPos + UTF8Length(CommentBegin),
        MaxInt));
      aValue := UTF8Trim(UTF8Copy(Str, 1, aPos - 1));
    end
    else
    begin
      aValue := Str;
      aComment := '';
    end;
  end;

  procedure RemoveTralingEmptyLines(aSection: cCHXIniSection);
  begin
          while (aSection.Lines.Count > 0) and
          (aSection.Lines[aSection.Lines.Count-1].Key = '') and
          (aSection.Lines[aSection.Lines.Count-1].Value = '') and
          (aSection.Lines[aSection.Lines.Count-1].Comment = '') do
            aSection.Lines.Delete(aSection.Lines.Count-1);

  end;

var
  i, aPos: integer;
  CurrLine: string;
  CurrSection: cCHXIniSection;
  aKey, aValue, aComment: string;
begin
  // Strings before any SectionByName. Internally managed as a Section
  //   with empty name.
  CurrSection := cCHXIniSection.Create('', '');
  SectionList.Add(CurrSection);

  i := 0;
  while i < aStringList.Count do
  begin
    CurrLine := UTF8Trim(aStringList[i]);
    aKey := '';
    aValue := '';
    aComment := '';

    if CurrLine = '' then
      // It's an empty line
      CurrSection.AddLine('', '', '', MergeKeys)
    else
    begin
      ExtractInlineComment(CurrLine, aValue, aComment);

      // Is it a Section?
      aPos := UTF8Pos(SectionEnd, aValue);
      if (UTF8Copy(aValue, 1, UTF8Length(SectionBegin)) = SectionBegin)
        and (aPos <> 0) then
      begin
        // Yes, it's a section

        // Removing last empty lines of the previous Section
        RemoveTralingEmptyLines(CurrSection);

        // Extracting Section Key
        aKey := UTF8Trim(UTF8Copy(aValue, UTF8Length(SectionBegin) + 1,
          aPos - 1 - UTF8Length(SectionBegin)));
        // Adding text after SectionEnd to aComment;
        aComment := UTF8Trim(UTF8Copy(aValue, aPos + UTF8Length(SectionEnd),
          MaxInt)) + aComment;

        if MergeSections then
        begin
          CurrSection := SectionByName(aKey);
          if CurrSection = nil then
          begin
            CurrSection := cCHXIniSection.Create(aKey, aComment);
            SectionList.Add(CurrSection);
          end
          else
           CurrSection.Comment := CurrSection.Comment + LineEnding+
            CommentBegin + aComment;
        end
        else
        begin
          CurrSection := cCHXIniSection.Create(aKey, aComment);
          SectionList.Add(CurrSection);
        end;
      end
      else
      begin
        // No, it isn't a section
        // Is it a "key=value" line?
        aPos := UTF8Pos(AssignChar, aValue);
        if aPos <> 0 then
        begin
          // Yes, it's a "key=value" line
          aKey := UTF8Trim(UTF8Copy(aValue, 1, aPos - 1));
          aValue := UTF8Trim(UTF8Copy(aValue, aPos + UTF8Length(AssignChar),
            Maxint));
        end;
        CurrSection.AddLine(aKey, aValue, aComment, MergeKeys);
      end;
    end;
    Inc(i);
  end;

  // Removing last empty lines
  RemoveTralingEmptyLines(CurrSection);
end;

procedure cCHXIni.LoadFromFile(const aFilename: string);
var
  StrList: TStringList;
begin
  StrList := TStringList.Create;
  try
    FileName := aFilename;
    StrList.LoadFromFile(UTF8ToSys(aFilename));
    { TODO -oChixpy : Join trucated lines, if EscapeLF = true}
    FillSectionList(StrList);
  finally
    FreeAndNil(StrList);
  end;
end;

procedure cCHXIni.SaveToFile(const aFilename: string);
var
  StrList: TStringList;
  CurrSection: cCHXIniSection;
  CurrLine: cCHXIniLine;
  aLine: string;
  i, j: integer;
begin
  StrList := TStringList.Create;
  try
    i := 0;
    aLine := ''; // Warning...
    while i < SectionList.Count do
    begin
      CurrSection := SectionList.Items[i];
      if (i <> 0) or (CurrSection.Name <> '') then
      begin
        // aLine is holding the last line
        if (aLine <> '') and (StrList.count > 0) then
          // Adding a empty line between sections if there is not one already
          //   or it's de first line of the file...
           StrList.Add('');
        aLine := SectionBegin + CurrSection.Name + SectionEnd;
        if CurrSection.Comment <> '' then
          aLine := aline + ' ' + CommentBegin + CurrSection.Comment;
        StrList.Add(aLine);
      end;

      j := 0;
      while j < CurrSection.Lines.Count do
      begin
        CurrLine := CurrSection.Lines[j];
        aLine := '';
        if CurrLine.key <> '' then
          aLine := aLine + CurrLine.Key + self.AssignChar;
        if CurrLine.Value <> '' then
          aLine := aLine + CurrLine.Value;
        if CurrLine.Comment <> '' then
          if aLine <> '' then
            aLine := aLine + ' ' + CommentBegin + ' ' + CurrLine.Comment
          else
            aLine := CommentBegin + ' ' + CurrLine.Comment;
        { TODO -oChixpy : Use EscapeLF to write the file }
        { Si EscapeLF, partir la línea en varias. }
        StrList.Add(UTF8ToSys(aLine));
        Inc(j);
      end;
      Inc(i);
    end;

    FileName := aFilename;
    StrList.SaveToFile(UTF8ToSys(aFilename));
  finally
    FreeAndNil(StrList);
  end;
end;

function cCHXIni.SectionNameList: TStringList;
var
  i: Integer;
begin
  Result := nil;

  if SectionCount = 0 then Exit;

  Result := TStringList.Create;

  i := 0;
  while i < SectionList.Count do
  begin
    Result.Add(SectionByIndex(i).Name);
    Inc(i);
  end;
end;

function cCHXIni.SectionByIndex(aIndex: integer): cCHXIniSection;
begin
  result := SectionList.Items[aIndex];
end;

function cCHXIni.SectionByName(aSectionName: string): cCHXIniSection;
var
  i: integer;
  aSection: cCHXIniSection;
begin
  Result := nil;
  i := 0;
  while i < SectionList.Count do
  begin
    aSection := SectionByIndex(i);

    if UTF8CompareText(aSection.Name, aSectionName) = 0 then
    begin
      Result := aSection;
      break;
    end;
    Inc(i);
  end;
end;

function cCHXIni.SectionCount: integer;
begin
  Result := SectionList.Count;
end;

function cCHXIni.ReadString(const aSectionName, aKeyName, aDefault: string): string;
var
  aSection: cCHXIniSection;
  aKey: cCHXIniLine;
begin

ashgdjhasgdjhgaskjdg

  Result := aDefault;
  aSection := SectionByName(aSectionName);
  if aSection = nil then
    Exit;
  aKey := aSection.LineByKey(aKeyName);
  if aKey = nil then
    Exit;
  Result := aKey.Value;

  // Is it quoted?
  if (RemoveQuotes) and (UTF8Copy(Result, 1, length(QuoteBegin)) = QuoteBegin) then
  begin
    // Seems that yes...
    // Big trick here....
    Result := QuoteEnd + UTF8Copy(Result, 1 + UTF8Length(QuoteBegin), Maxint);
    Result := AnsiDequotedStr(Result, QuoteEnd);
  end;
end;

function cCHXIni.ReadString(const aSectionIndex: integer; const aKeyName,
  aDefault: string): string;
var
  aSection: cCHXIniSection;
  aKeyIndex: integer;
begin
  Result := aDefault;

  aSection := Self.SectionList[aSectionIndex];
  aKey := aSection.LineByKey(aKeyName);
  if aKey = nil then
    Exit;
  Result := aKey.Value;

  // Is it quoted?
  if (RemoveQuotes) and (UTF8Copy(Result, 1, length(QuoteBegin)) = QuoteBegin) then
  begin
    // Seems that yes...
    // Big trick here....
    Result := QuoteEnd + UTF8Copy(Result, 1 + UTF8Length(QuoteBegin), Maxint);
    Result := AnsiDequotedStr(Result, QuoteEnd);
  end;
end;

function cCHXIni.ReadMultiStrings(const aSectionName, aKeyName, aDefault: string; const Separator: char): TStringList;
var
  aStr: string;
begin
  Result := TStringList.Create;
  Result.StrictDelimiter := true;
  Result.Delimiter := Separator;
  aStr := ReadString(aSectionName, aKeyName, '', False);
  if aStr = '' then
    aStr := aDefault;
  Result.DelimitedText := aStr;
end;

function cCHXIni.ReadInteger(const aSectionName, aKeyName: string;
  const aDefault: integer): integer;
begin
  Result := StrToIntDef(ReadString(aSectionName,aKeyName,''),aDefault);
end;

function cCHXIni.ReadTPoint(const aSectionName, aKeyName: string;
  const aDefault: TPoint): TPoint;
var
  aSL: TStringList;
begin
    aSL := ReadMultiStrings(aSectionName, aKeyName, IntToStr(aDefault.x) + ','
      + IntToStr(aDefault.y));
    try
      Result.x := StrToInt(aSL[0]);
      Result.y := StrToInt(aSL[1]);
    except
      Result.x := aDefault.x;
      Result.y := aDefault.y;
    end;
    FreeAndNil(aSL);
end;

procedure cCHXIni.WriteString(const aSectionName, aKeyName, aValue: string);
var
  aSection: cCHXIniSection;
  aKey: cCHXIniLine;
begin
  aSection := SectionByName(aSectionName);
  if aSection = nil then
  begin
    aSection:= cCHXIniSection.Create(aSectionName, '');
    SectionList.add(aSection);
  end;
  aKey := aSection.LineByKey(aKeyName);
  if aKey = nil then
    aKey:= aSection.AddLine(aKeyName,aValue,'',MergeKeys)
  else
    aKey.Value := aValue;
end;

procedure cCHXIni.WriteInteger(const aSectionName, aKeyName: string;
  const aValue: integer);
begin
  WriteString(aSectionName,aKeyName, IntToStr(aValue));
end;


constructor cCHXIni.Create;
begin
  inherited Create;

  // Some default values:
  self.CommentBegin := ';';
  self.SectionBegin := '[';
  self.SectionEnd := ']';
  self.NewLine := '\n';
  self.QuoteBegin := '"';
  self.QuoteEnd := '"';
  self.AssignChar := '=';
  self.EscapeLFChar := '\';
  self.EscapeLFMaxWidth := 80;

  FSectionList := cCHXIniSectionList.Create(true);
end;

destructor cCHXIni.Destroy;
begin
  FreeAndNil(FSectionList);
  inherited Destroy;
end;

end.

