{
[Info]
This script creates a Emuteca database from a TOSEC .dat.

Imports title, versión, year, publisher, dumpinfo, etc. from filenames.

It doesn't add any parent info, so when importing data groups are keeped.
[Data]
Name=Chixpy
Version=0.01
Date=20170923
[Changes]

[EndInfo]
}
program TOSECImport;

//uses uTOSECUtils;

{$I '../Units/uTOSECUtils.pas'}

var
  DBFilename, aStr: string;
  TOSECFileNames, TOSECFile, SoftList, DBList: TStringList;
  i, j: LongInt;
begin
  TOSECFileNames := CreateStringList;
  TOSECFileNames.Duplicates := dupIgnore;

  AskMultiFile(TOSECFileNames,
    'Select TOSEC File(s)',
    'TOSEC databases (*.dat)|*.dat', '');

  if TOSECFileNames.Count = 0 then
  begin
    WriteLn('Error: No TOSEC files selected.');
    TOSECFileNames.Free;
    Exit;
  end;

  DBFilename := AskFile('Database file for output',
    'Emuteca soft DB|' + krsFileMaskSoft,
    '');

  if DBFilename = '' then
  begin
    WriteLn('Error: No Database file selected.');
    TOSECFileNames.Free;
    Exit;
  end;

  TOSECFile := CreateStringList;
  SoftList := CreateStringList;
  SoftList.Duplicates := dupIgnore;
  SoftList.Sorted := True;
  DBList := CreateStringList;
  try
    for j := 0 to TOSECFileNames.Count -1 do
    begin
      if not FileExistsUTF8(TOSECFilenames[j]) then
      begin
        WriteLn('');
        WriteLn('ERROR: ' + TOSECFilenames[j] + ' not found.');
        WriteLn('');
        Continue;
      end;

      WriteLn('Reading: "' + TOSECFilenames[j] + '"');
      TOSECFile.LoadFromFile(UTF8ToSys(TOSECFilenames[j]));
      WriteLn(IntToStr(TOSECFile.Count) + ' lines readed.');
      WriteLn('');
      WriteLn('Analizing file...');

      i := 0;
      while i < TOSECFile.Count do
      begin
        aStr := TOSECExtractSoftLine(TOSECFile[i], 'sha1');
        // aStr = <SHA1>|<SoftName>

        if aStr <> '' then
          SoftList.Add(aStr);

        if (i and 1023) = 1023 then
          WriteLn(IntToStr(i) + ' lines analized.');
        Inc(i);        
      end; 
      WriteLn('File analized.');
      WriteLn('');
    end;

    WriteLn('');
    WriteLn(IntToStr(SoftList.Count) + ' soft files found.');
    WriteLn('');

    i := 0;
    while i < SoftList.Count do
    begin
      aStr := TOSECExtractInfo(SoftList[i]);

      if aStr <> '' then
        DBList.Add(aStr);

      if (i and 1023) = 1023 then
        WriteLn(IntToStr(i) + ' soft files analized.');
      Inc(i);
    end;

    WriteLn('');
    WriteLn('Saving... ' + DBFilename);
    DBList.Sort;
    DBList.Insert(0, krsCSVSoftHeader) //Adding header
    DBList.SaveToFile(UTF8ToSys(DBFilename));
  finally
    DBList.Free;
    SoftList.Free;
    TOSECFile.Free;
    TOSECFileNames.Free;
  end;

  WriteLn('');
  WriteLn('DONE');
  WriteLn('====');
end.
