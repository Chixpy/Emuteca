{
[Info]
This script creates a Emuteca database from a TOSEC .dat.

Imports title, versión, year, publisher, dumpinfo, etc. from filenames.

It doesn't add any parent info, so when importing data groups are keeped.
[Author]
Name=Chixpy
Date=20170918
[EndInfo]
}
program TOSECImport;

//uses uTOSECUtils;

{$I '../Units/uTOSECUtils.pas'}

var
  TOSECFilename, DBFilename, aStr: string;
  TOSECFile, SoftList, DBList: TStringList;
  i: LongInt;
begin
  TOSECFilename := AskFile(
    'TOSEC File',
    'TOSEC databases (*.dat)|*.dat', '');
	
  if not FileExistsUTF8(TOSECFilename) then
  begin
    WriteLn('The file "' + TOSECFilename + '" not found.');
  end;

  DBFilename := AskFile('Database file for output',
    'Emuteca soft DB|' + krsFileMaskSoft,
    ExtractFilenameOnly(TOSECFilename) + krsFileExtSoft);
  
  TOSECFile := CreateStringList;
  SoftList := CreateStringList;
  DBList := CreateStringList;
  try
    DBList.Add(krsCSVSoftHeader) //Adding header

    WriteLn('Reading: "' + TOSECFilename + '"');  
    TOSECFile.LoadFromFile(TOSECFilename);
	WriteLn('');
    WriteLn(IntToStr(TOSECFile.Count) + ' lines readed.');  	
    WriteLn('');
    WriteLn('Analizing file...');

    i := 0;
    while i < TOSECFile.Count do
    begin
      aStr := TOSECExtractSoftLine(TOSECFile[i], 'sha1');
      // aStr = <SHA1>,<SoftName>

      if aStr <> '' then
        SoftList.Add(aStr);

      if (i and 1023) = 1023 then
        WriteLn(IntToStr(i) + ' lines analized.');
      Inc(i);
    end;

    WriteLn('');
    WriteLn(IntToStr(SoftList.Count) + ' soft files found.');
    WriteLn('');

    SoftList.Sort;
    i := 0;
    while i < SoftList.Count do
    begin
      aStr := TOSECExtractInfo(SoftList[i], True);

      if aStr <> '' then
        DBList.Add(aStr);

      if (i and 1023) = 1023 then
        WriteLn(IntToStr(i) + ' soft files analized.');
      Inc(i);
    end;

    WriteLn('');
    WriteLn('Saving... ' + DBFilename);
    DBList.SaveToFile(DBFilename);
  finally
    DBList.Free;
    SoftList.Free;
    TOSECFile.Free;
  end;

  WriteLn('');
  WriteLn('DONE');
  WriteLn('====');
end.
