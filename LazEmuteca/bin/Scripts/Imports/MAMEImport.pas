{
[Info]
This script creates a database from MAME.
It needs two txt files created with:
* mame -listfull > MAMEfull.txt
* mame -listclones > MAMEclones.txt
[Author]
Name=Chixpy
Date=20170919
[EndInfo]
}
program MAMEImport;

const
  MaxInt = 65535; // Only used for copying to the end of line
  LengthClonID = 17; // Lenght of first column in clones list

function TestFilename(aFilename: string): boolean;
begin
  Result := FileExistsUTF8(aFilename);
  if not Result then
    WriteLn('The file "' + aFilename + '" not found.');
end;

var
  FullFilename, ClonesFilename, OutFilename: string;
  FullList, ClonesList, OutList, ParentList: TStringList;
  aVer, aId, aName, aParent: string;
  aPos, i, j: integer;

begin
  FullFilename := AskFile(
    'File with full data (mame -listfull > MAMEfull.txt)',
    'All files (*.*)|*.*', '');

  if not TestFilename(FullFilename) then Exit;

  ClonesFilename := AskFile(
    'File with clones data (mame -listclones > MAMEclones.txt)',
    'All files (*.*)|*.*', ExtractFilePath(FullFilename) + 'MAMEclones.txt');

  if not TestFilename(ClonesFilename) then Exit;

  OutFilename := AskFile('Database file for output',
    'Emuteca soft DB|' + krsFileMaskSoft, 'MAME' + krsFileExtSoft);

  WriteLn('Reading files, this can take a while...');
  WriteLn('');

  FullList := CreateStringList;
  ClonesList := CreateStringList;
  OutList := CreateStringList;
  ParentList := CreateStringList;
  try
    FullList.LoadFromFile(FullFilename);
    FullList.Delete(0); // Removing header
    FullList.Sort;
    ClonesList.LoadFromFile(ClonesFilename);
    ClonesList.Delete(0); // Removing header
    ClonesList.Sort;

    OutList.Add(krsCSVSoftHeader) //Adding header

    ParentList.Add(krsCSVGroupHeader) //Adding header

    WriteLn('Creating import list...');
    WriteLn('');
    i := 0; // FullList line count
    j := 0; // ClonesList line count
    while i < FullList.Count do
    begin
      aVer := FullList[i]; // Temp

      aPos := Pos('"', aVer);
      aID := Trim(Copy(aVer, 1, aPos - 1));
      aName := Trim(Copy(aVer, aPos + 1, MaxInt)); // Removing first '"'
      aName := Trim(Copy(aName, 1, Length(aName) - 1)); // Removing last '"'

      // Spliting Name / Version
      aPos := Pos('(', aName);
      if aPos >= 1 then
      begin
        aVer := Trim(Copy(aName, aPos, MaxInt));
        aName := Trim(Copy(aName, 1, aPos - 1));
      end
      else
        aVer := '';

      // Getting parent ID
      aParent := '';
      if j < ClonesList.Count then
        aPos := CompareText(aID, Trim(Copy(ClonesList[j], 1,
          LengthClonID)))
      else
        aPos := -1;

      while aPos > 0 do
      begin
        if j < ClonesList.Count then
        begin
          Inc(j);
          aPos := CompareText(aID, Trim(Copy(ClonesList[j], 1,
            LengthClonID)))
        end
        else
          aPos := -1;
      end;

      if aPos = 0 then // found
        aParent := Trim(Copy(ClonesList[j], LengthClonID, MaxInt))
      else
      begin
        aParent := aID; // it's a parent itself
        
        // "ID","Title","Sort title","Year","Developer","Media file"
        ParentList.Add('"' + aParent + '","' + aName + '",' + 
		  krsImportKeepValue + ',' + krsImportKeepValue + ',' + 
		  krsImportKeepValue + ',"' + aParent + '"');
        // Adding to parent list
      end;

      // "Group","SHA1","ID","Folder","FileName","Title","TransliteratedName",
      // "SortTitle","Version","Year","Publisher","Zone","DumpStatus",
      // "DumpInfo","Fixed","Trainer","Translation","Pirate","Cracked",
      // "Modified","Hack"

      OutList.Add('"' + aParent + '",' + krsImportKeepValue + ',"' + aID +
        '",' + krsImportKeepValue + ',' + krsImportKeepValue + ',"' + aName +
        '",' + krsImportKeepValue + ',' + krsImportKeepValue + ',"' + aVer +
        '",' + krsImportKeepValue + ',' + krsImportKeepValue + ',' + 
        krsImportKeepValue + ',' + krsImportKeepValue + ',' + 
        krsImportKeepValue + ',' + krsImportKeepValue + ',' +
        krsImportKeepValue + ',' + krsImportKeepValue + ',' +
        krsImportKeepValue + ',' + krsImportKeepValue + ',' +
        krsImportKeepValue + ',' + krsImportKeepValue);
      Inc(i);
    end;

    WriteLn('Saving files...');
    WriteLn(OutFilename);
    // Don't work? aParent := ChangeFileExt(OutFilename, krsEmutecaGroupFileExt);
    aParent := Copy(OutFilename, 1, Length(OutFilename) - 4) +
      krsFileExtGroup;
    WriteLn(aParent);

    OutList.SaveToFile(OutFilename);
    ParentList.SaveToFile(aParent);

    WriteLn('');
    WriteLn('Done.');

  finally
    FullList.Free;
    ClonesList.Free;
    OutList.Free;
    ParentList.Free
  end;
end.
