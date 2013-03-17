{ Unit with some string related functions. }
unit uCHXStrUtils;

{$DEBUGINFO OFF}

interface

uses Classes, Strutils, SysUtils, FileUtil, LazUTF8, LazUTF8Classes,
  uCHXConst, uCHXRscStr;


// STRING UTILS
// ------------
function RemoveFromBrackets(const aString: string): string;
{< Removes text from the first '(' o '[' found in the aString. }

function TextSimilarity(const aString1, aString2: string): byte;
{< Returns the similarity between 2 strings.

  Based in http://www.catalysoft.com/articles/StrikeAMatch.html method tweaked
    a little.
}

procedure WriteStringToStream(AStream: TStream);
{< Writes a strint to a stream.
}
procedure ReadStringFromStream(AStream: TStream);
{< Writes a strint to a stream.
}

// DIRECTORY NAME UTILS
// --------------------
function SetAsFolder(const aValue: string): string;
{< Adds PathDelim at the end of string and changes it to '/'

  IncludeTrailingPathDelimiter includes the PathDelim even if aValue = ''.
    This function non't add it in this case, so when testing if it's empty
    we don't need @code (@(aFolder=''@) or @(aFolder=PathDelim@))

  In the other hand, paths are converted to Linux one as Windows AND
    MS-DOS (+2.0) recognise without problem
}
function SysPath(const aPath: string): string;
function WinPath(const aPath: string): string;
function UnixPath(const aPath: string): string;

// FILENAME UTILS
// ---------------
function CleanFileName(const AFileName: string): string;
{< Changes some invalid characters in filenames.
}
function SetAsRelativeFile(const aFileName: string; BaseDir: string): string;

function SetAsFile(const aFileName: string): string;

// TSTRINGLIST UTILS
// ----------------------

procedure CleanStringList(aStringList: TStrings;
  CommentChar: string = ';');
{< Removes comments and empty lines from a TStringList.
}

function AddToStringList(aList: TStrings; aString: String): integer;
{< Add a String to a StringList.

  Don't add repeated strings.
}

// UTILIDADES VARIAS
// -----------------

function StrCount(aString, ToSearch: string;
  CaseSensitve: boolean = False): cardinal;
{< Counts the times that a substring is in a string.

  NOTE: StrCount('ooo', 'oo') = 2 .
}

function StrToCardinal(const aString: String): cardinal;

function StrToCardinalDef(const aString: String;
  const Default: cardinal): cardinal;

function SecondsToFmtStr(aValue: int64): String;


implementation

// STRING UTILS
// ------------
function RemoveFromBrackets(const aString: string): string;
var
  Position: integer;
begin
  Result := ExtractFileNameOnly(aString);
  Position := UTF8Pos('(', Result);
  if Position <> 0 then
    Result := UTF8Copy(Result, 1, Position - 1);
  Position := UTF8Pos('[', Result);
  if Position <> 0 then
    Result := UTF8Copy(Result, 1, Position - 1);

  Result := Trim(Result);
end;

function TextSimilarity(const aString1, aString2: string): byte;

  procedure LetterPairs(aStrList: TStrings; const aString: string);
  var
    i: integer;
    CurrPair: string;
    CharUTF8: string;
  begin
    if aStrList = nil then
      aStrList := TStringListUTF8.Create
    else
      aStrList.Clear;

    i := 1;
    while i < UTF8Length(aString) do
    begin
      CurrPair := UTF8Copy(aString, i, 2);

      if UTF8Length(CurrPair) <> 2 then
      begin
        Inc(i);
        Continue;
      end;

      // Removing some separators...
      if CurrPair[1] in kCUUTF8Delimiters then
      begin
        Inc(i);
        Continue;
      end;

      CharUTF8 := UTF8Copy(CurrPair, 2, 1);
      if CharUTF8[1] in kCUUTF8Delimiters then
      begin
        CurrPair := UTF8Copy(CurrPair, 1, 1);
      end;

      aStrList.Add(CurrPair);
      Inc(i, Length(CurrPair));
    end;
  end;

var
  StrList1, StrList2: TStringListUTF8;
  CurrPair: string;
  i, j: integer;
  Intersection: integer;
  Union: integer;
begin
  Result := 0;
  if (aString1 = '') or (aString2 = '') then
    Exit;

  StrList1 := TStringListUTF8.Create;
  StrList2 := TStringListUTF8.Create;
  StrList1.CaseSensitive := False;
  StrList2.CaseSensitive := False;
  try
    LetterPairs(StrList1, UTF8UpperCase(aString1));
    StrList1.Sort;
    LetterPairs(StrList2, UTF8UpperCase(aString2));
    StrList2.Sort;

    Intersection := 0;
    Union := StrList1.Count + StrList2.Count;

    i := StrList1.Count - 1;
    while i >= 0 do
    begin
      CurrPair := StrList1[i];
      j := StrList2.IndexOf(CurrPair);
      if j <> -1 then
      begin
        StrList2.Delete(j);
        Inc(Intersection, 2);
      end;
      Dec(i);
    end;
  finally
    FreeAndNil(StrList1);
    FreeAndNil(StrList2);
  end;

  if Union <> 0 then
    Result := Round(Intersection / Union * 100);
end;

procedure WriteStringToStream(AStream: TStream);
begin

end;

procedure ReadStringFromStream(AStream: TStream);
begin

end;

// DIRECTORY NAME UTILS
// --------------------
function SetAsFolder(const aValue: String): String;
begin
  Result := SysPath(aValue);

  // For Emuteca, always relative...
  if FilenameIsAbsolute(Result) then
    Result := CreateRelativePath(Result, SysPath(GetCurrentDirUTF8), false);

  { Always with TrailingPathDelimiter, but only if it's not empty or root }
  if ExcludeTrailingPathDelimiter(Result) <> '' then
    Result := IncludeTrailingPathDelimiter(Result);

  // I like UNIX PathSep (and it's better for cross-configuring)
  Result := UnixPath(Result);
end;

function SetAsRelativeFile(const aFileName: string; BaseDir: string): string;
begin
  // CreateRelativePath doesn't like Unix Style under Windows... :-(
  Result := CreateRelativePath(SysPath(aFileName), SysPath(BaseDir), False);

  Result := UnixPath(Result);
end;

function SetAsFile(const aFileName: string): string;
begin
  Result := UnixPath(aFileName);
end;

function SysPath(const aPath: string): string;
begin
  {$IFDEF Windows}
  Result := WinPath(aPath);
  {$ELSE}
  Result := UnixPath(aPath);
  {$ENDIF}
end;

function WinPath(const aPath: string): string;
var
  i: integer;
begin
  // Seems to be faster than StringReplace...
  Result := aPath;
  i := Length(Result);
  while i > 0 do
  begin
    if Result[i] = '/' then
      Result[i] := '\';
    Dec(i);
  end;
end;

function UnixPath(const aPath: string): string;
var
  i: integer;
begin
  // Seems to be faster than StringReplace...
  Result := aPath;
  i := Length(Result);
  while i > 0 do
  begin
    if Result[i] = '\' then
      Result[i] := '/';
    Dec(i);
  end;
end;

// FILE NAME UTILS
// ---------------
function CleanFileName(const AFileName: string): string;
begin
  // Windows (and Linux) invalid characters
  Result := AnsiReplaceText(AFileName, '?', '_');
  Result := AnsiReplaceText(Result, '*', '-');
  Result := AnsiReplaceText(Result, '"', '_');
  Result := AnsiReplaceText(Result, '\', '-');
  Result := AnsiReplaceText(Result, '/', '-');
  Result := AnsiReplaceText(Result, '|', '-');
  Result := AnsiReplaceText(Result, '<', '-');
  Result := AnsiReplaceText(Result, '>', '-');

  // Playing with ":"
  Result := AnsiReplaceText(Result, ' : ', ' - ');
  Result := AnsiReplaceText(Result, ': ', ' - ');
  Result := AnsiReplaceText(Result, ' :', ' - ');
  Result := AnsiReplaceText(Result, ':', ' - ');

  Result := Trim(Result);
end;

// UTILIDADES TSTRINGLIST
// ----------------------

procedure CleanStringList(aStringList: TStrings;
  CommentChar: string = ';');
var
  Cont: cardinal;
begin
  if aStringList = nil then
    Exit;

  for Cont := aStringList.Count - 1 downto 0 do
  begin
    if Pos(CommentChar, aStringList.Strings[Cont]) <> 0 then
      aStringList.Strings[Cont] :=
        Copy(aStringList.Strings[Cont], 1, Pos(CommentChar,
        aStringList.Strings[Cont]) - 1);
    aStringList.Strings[Cont] := Trim(aStringList.Strings[Cont]);
    if aStringList.Strings[Cont] = '' then
      aStringList.Delete(Cont);
  end;
end;

function AddToStringList(aList: TStrings; aString: String): integer;
begin
  Result := -1;
  aString := Trim(aString);
  if (aList = nil) or (aString = '') then
    Exit;
  Result := aList.IndexOf(aString);
  if Result = -1 then
    Result := aList.Add(aString);
end;

// UTILIDADES VARIAS
// -----------------

function StrCount(aString, ToSearch: string;
  CaseSensitve: boolean = False): cardinal;
var
  Cont: cardinal;
  TempCadena: string;
begin
  Result := 0;
  if not CaseSensitve then
  begin
    aString := AnsiUpperCase(aString);
    ToSearch := AnsiUpperCase(ToSearch);
  end;

  for Cont := 1 to Length(aString) do
  begin
    TempCadena := Copy(aString, Cont, Length(ToSearch));
    if TempCadena = ToSearch then
      Result := Result + 1;
  end;
end;

function StrToCardinalDef(const aString: String;
  const Default: cardinal): cardinal;
var
  h: int64;
begin
  h := StrToInt64Def(aString, Default);
  if (h > High(cardinal)) or (h < 0) then
    h := Default;
  Result := h;
end;

function SecondsToFmtStr(aValue: int64): String;
begin
  Result := RightStr('00' + IntToStr(aValue mod 60), 2);
  aValue := aValue div 60;
  Result := RightStr('00' + IntToStr(aValue mod 60), 2) + ':' + Result;
  aValue := aValue div 60;
  Result := IntToStr(aValue) + ':' + Result;
end;

function StrToCardinal(const aString: String): cardinal;
var
  h: int64;
begin

  h := StrToInt64(aString);
  if (h > High(cardinal)) or (h < 0) then
    raise EConvertError.CreateFmt(rsCUExcCardRange, [h]);
  Result := h;
end;


end.
