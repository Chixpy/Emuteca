{
[Info]
This script creates a Emuteca database from a TOSEC .dat.

Imports title, versión, year, publisher, dumpinfo, etc. from filenames.

Don't changes groups.
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
  DBFilename := AskFile('Database file for output',
    'Emuteca soft DB|' + krsFileMaskSoft,
    '');
  if DBFilename = '' then
  begin
    WriteLn('Error: No Database file selected.');
    Exit;
  end;

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

  TOSECFile := CreateStringList;
  SoftList := CreateStringList;
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
      TOSECReadDatFile(TOSECFile, SoftList, 'sha1');
      WriteLn('File analisis finalized.');
      WriteLn('');
    end;

    SoftList.Sort;
    WriteLn('');
    WriteLn(IntToStr(SoftList.Count) + ' soft files found.');
    WriteLn('');

    DBList.BeginUpdate;
    i := 0;
    while i < SoftList.Count do
    begin
      aStr := TOSECExtractInfo(SoftList[i]);

      if aStr <> '' then
        DBList.Add(aStr);

      if (i and 511) = 511 then
        WriteLn(IntToStr(i) + ' soft files analized.');
      Inc(i);
    end;
    DBList.EndUpdate;
    
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
