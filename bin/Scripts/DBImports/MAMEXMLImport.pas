{
[Info]
This script creates a database from MAME XML. XML file can be created with

MAME -listxml > mame.xml

or downloaded from MAMEDevs page (maybe this is faster)

It's better than -listfull + -listclones, because
year and manufacturer can be extracted

[Data]
Name=Chixpy
Version=0.01
Date=20181
[Changes]
0.01
  + Initial working version
[EndInfo]
}
program MAMEImport;

function TestFilename(aFilename: string): boolean;
begin
  Result := FileExistsUTF8(aFilename);
  if not Result then
    WriteLn('The file "' + aFilename + '" not found.');
end;

var
  XMLFilename, OutFilename: string;
  XMLFile, ClonesList, OutList, ParentList: TStringList;
  aLine, aVer, aID, aName, aParent, aPirate, aZone, aHack: string;
  aPos: integer;

begin
  OutFilename := AskFile('Database file for output',
    'Emuteca soft DB|' + krsFileMaskSoft, 'MAME' + krsFileExtSoft);
  if OutFilename = '' then
  begin
    WriteLn('Output file not assigned.');
    Exit;
  end;

  XMLFilename := AskFile(
    'XML with mame data',
    'XML files (*.xml)|*.xml', '');
  if not TestFilename(XMLFilename) then Exit;


  WriteLn('Reading files, this can take a while...');
  WriteLn('');

  XMLFile := CreateStringList;
  ClonesList := CreateStringList;
  OutList := CreateStringList;
  ParentList := CreateStringList;
  try
    WriteLn ('Reading: ' + XMLFilename);
    XMLFile.BeginUpdate;
    XMLFile.LoadFromFile(XMLFilename);
    XMLFile.EndUpdate;
	
    OutList.Add(krsCSVSoftHeader) //Adding header
    ParentList.Add(krsCSVGroupHeader) //Adding header
	
	WriteLn('');
    WriteLn('Creating import list...');
    WriteLn('');
	
	
	while XMLFile.Count > 0 do
    begin
	  aLine := Trim(XMLFile[0]);
	  XMLFile.Delete(0);
	  
	  if Pos('<machine', aLine) > 0 then
	  begin	 

        // Joining next lines until end of tag	  
	    aPos := Pos('>', aLine);
        // TODO: Check file end.
        while (aPos < 1) do
        begin
          aLine := aLine + Trim(XMLFile[0]);
	      XMLFile.Delete(0);
          aPos := Pos('>', aLine);
        end;
		
	    // Searching "name=" key for Title
        // and joning next lines if not found
        aPos := Pos('name="', aLine);
		
        aPos := Pos('cloneof="', aLine);

		
		Continuar buscado si es clon de algo
	  end	  
	end;

	
  finally
    XMLFile.Free;
    ClonesList.Free;
    OutList.Free;
    ParentList.Free
  end;

  Continuar desarrollando...

  ClonesList := CreateStringList;
  OutList := CreateStringList;
  ParentList := CreateStringList;
  try
    WriteLn ('Reading: ' + FullFilename);
    FullList.BeginUpdate;
    FullList.LoadFromFile(FullFilename);
    FullList.Delete(0); // Removing header
    FullList.Sort; // Sorting for faster search
    FullList.EndUpdate;

    WriteLn ('Reading: ' + ClonesFilename);
    ClonesList.BeginUpdate;
    ClonesList.LoadFromFile(ClonesFilename);
    ClonesList.Delete(0); // Removing header
    ClonesList.Sort; // Sorting for faster search
    ClonesList.EndUpdate;

    OutList.Add(krsCSVSoftHeader) //Adding header

    ParentList.Add(krsCSVGroupHeader) //Adding header

    WriteLn('');
    WriteLn('Creating import list...');
    WriteLn('');

    while FullList.Count > 0 do
    begin
      aName := FullList[0];
      FullList.Delete(0);

      aPos := Pos('"', aName);
      if aPos > 1 then
      begin
        // Extracting ID
        aID := Trim(Copy(aName, 1, aPos - 1));

        // Extracting Title (+ Version)
        aName := Trim(Copy(aName, aPos + 1, 1000)); // Removing first '"'
        aName := Trim(Copy(aName, 1, Length(aName) - 1)); // Removing last '"'

        if (FullList.Count and 1023) = 1023 then
          WriteLn('Reading: ' + aName +  '(' + IntToStr(FullList.Count) + ' left.)');

        // Spliting Name / Version
        aPos := Pos('(', aName);
        if aPos >= 1 then
        begin
          aVer := Trim(Copy(aName, aPos, 1000));
          aName := Trim(Copy(aName, 1, aPos - 1));
        end
        else
          aVer := '';

        // TODO: Try to get some more and better data:
        //   - Version from name
        //   - Year and Manufacturer can be get from MAME somehow
        
        // Bootlegs / Pirate
        aPirate := '';
        if Pos('bootleg', aVer) > 0 then
          aPirate := 'Bootleg';
         
        aZone := krsImportKeepValue;
        if Pos('World', aVer) > 0 then
          aZone := 'xw'
        else if Pos('JUE', aVer) > 0 then
          aZone := 'xw'
        else if Pos('Euro', aVer) > 0 then
          aZone := 'eu'        
        else if Pos('Spain', aVer) > 0 then
          aZone := 'sp' 
        else if Pos('Spanish', aVer) > 0 then
          aZone := 'sp' 
        else if Pos('France', aVer) > 0 then
          aZone := 'fr' 
        else if Pos('French', aVer) > 0 then
          aZone := 'fr' 
        else if Pos('Italy', aVer) > 0 then
          aZone := 'it' 
        else if Pos('Italian', aVer) > 0 then
          aZone := 'it' 
//        else if Pos('Germany', aVer) > 0 then
//          aZone := 'de' 
        else if Pos('German', aVer) > 0 then
          aZone := 'de' 
//        else if Pos('USA', aVer) > 0 then
//          aZone := 'us'        
        else if Pos('US', aVer) > 0 then
          aZone := 'us'       
        else if Pos('Asia', aVer) > 0 then
          aZone := 'xa'        
        else if Pos('Japan', aVer) > 0 then
          aZone := 'jp'        
        else if Pos('Korea', aVer) > 0 then
          aZone := 'kr'        
//        else if Pos('Korean', aVer) > 0 then
//          aZone := 'kr'        
        else if Pos('China', aVer) > 0 then
          aZone := 'cn'        
        else if Pos('Hong Kong', aVer) > 0 then
          aZone := 'hk'        
        else if Pos('Taiwan', aVer) > 0 then
          aZone := 'tw'        
        else if Pos('Brazil', aVer) > 0 then
          aZone := 'br'        
        else if Pos('Russia', aVer) > 0 then
          aZone := 'ru'
        else if Pos('New Zealand', aVer) > 0 then
          aZone := 'nz'; 
          

        // Hacked 
        aHack := krsImportKeepValue        
        if Pos('hack', aVer) > 0 then
          aHack := 'Hack';
          
          
        // Searching parent

        aParent := aID; // if ClonesList.Count = 0
        aPos := 1;
        while (ClonesList.Count > 0) and (aPos > 0) do
        begin
          aParent := ClonesList[0];
          LengthClonID := Pos(' ', aParent);

          aPos := CompareText(aID, Trim(Copy(aParent, 1, LengthClonID)));

          if aPos = 0 then // Found, set parent.
          begin
            aParent := Trim(Copy(aParent, LengthClonID, 1000));
          end
          else if aPos > 0 then // Not found, search next in CloneList
          begin
            ClonesList.Delete(0);
          end
          else // if aPos < 0 then
          begin // Not found, its a parent
            aParent := aID;

            // Adding to parent list
            // "ID","Title","Sort title","Year","Developer","Media file"
            ParentList.Add('"' + aParent + '","' + aName + '",' +
              krsImportKeepValue + ',' + krsImportKeepValue + ',' +
              krsImportKeepValue + ',"' + aParent + '"');
          end;
        end; // while (ClonesList.Count > 0) and (aPos > 0) do

        // "Group","SHA1","ID","Folder","FileName","Title","TransliteratedName",
        // "SortTitle","Version","Year","Publisher","Zone","DumpStatus",
        // "DumpInfo","Fixed","Trainer","Translation","Pirate","Cracked",
        // "Modified","Hack"

        OutList.Add('"' + aParent + '",' + krsImportKeepValue + ',"' + aID +
          '",' + krsImportKeepValue + ',' + krsImportKeepValue + ',"' + aName +
          '",' + krsImportKeepValue + ',' + krsImportKeepValue + ',"' + aVer +
          '",' + krsImportKeepValue + ',' + krsImportKeepValue + ',' +
          aZone + ',' + krsImportKeepValue + ',' +
          krsImportKeepValue + ',' + krsImportKeepValue + ',' +
          krsImportKeepValue + ',' + krsImportKeepValue + ',' +
          aPirate + ',' + krsImportKeepValue + ',' +
          krsImportKeepValue + ',' + krsImportKeepValue);
      end; // if Pos('"', aName) > 2 then
    end; // while FullList.Count > 0 do

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
