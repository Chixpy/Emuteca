{
[Info]
This script creates a Emuteca database from a TOSEC .dat.

Imports title, versión, year, publisher, dumpinfo, etc. from filenames.

Don't changes groups.
[Data]
Name=Chixpy
Version=0.02
Date=20221019
[Changes]
* 0.04 - 20230514
  c Changed progress log to a temporized one.
* 0.03 - 20200201
  f Adapting to new uTOSECUtils.pas (v0.12).
* 0.02 - 20200201
  a Displaying actual number of analized files.
* 0.01 - 20170923
  * Initial version
[EndInfo]
}
program TOSECImport;

//uses uTOSECUtils;
{$I '../Units/uTOSECUtils.pas'}

var
  DBFilename, aStr: string;
  TOSECFileNames, TOSECFile, SoftList, DBList: TStringList;
  i, j: LongInt;
  aTimer: TDateTime;
begin
  DBFilename := AskFile('Database file for output',
    'Emuteca soft DB|' + krsFileMaskSoft,
    '');
  if DBFilename = '' then
  begin
    WriteLn('Error: No Database file selected.');
    Exit;
  end;

  WriteLn('Output File: ' + DBFilename);

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
      if not FileExists(TOSECFilenames[j]) then
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

    aTimer := Now;
    DBList.BeginUpdate;
    i := 0;
    while i < SoftList.Count do
    begin
      aStr := TOSECExtractInfo(SoftList[i]);

      if aStr <> '' then
        DBList.Add(aStr);

      Inc(i);

      if SecondsBetween(Now, aTimer) > 15 then
      begin
        WriteLn(IntToStr(i) + ' soft files analized.');
        aTimer := Now;
      end;
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

  TOSECFinish;

  WriteLn('');
  WriteLn('DONE');
  WriteLn('====');
end.
