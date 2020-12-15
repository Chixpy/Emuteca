{ Emuteca Script
[Info]
This script renames files from a folder with MAME Software List names from other systems than arcade to full game names:
* Example from nes.xml: smb1.<ext> -> Super Mario Bros. (World).<ext>

Parenthesis are not removed, to do it use Misc/RemoveParenthesis.pas

Usefull for MAME image sets, as Emuteca DB by default uses game titles for searching media.

It needs the xml file from MAME's "hash" directory of the system.

[Data]
Name=Chixpy
Version=0.01
Date=
[Changes]
0.01
  + Initial working version
[EndInfo]
}
program MAMENames2FullName;

//uses uETKFileUtils, uETKStrUtils;
{$I '../Units/uETKFileUtils.pas'}
{$I '../Units/uETKStrUtils.pas'}

var
  aFolder, aFile, aExt, outFile, aID, aName, aLine: string;
  SLXMLFile, SLXMLList, aFileList: TStringList;
  i, aPos: integer;
  
begin
  aFile := AskFile(
    'Software List File)',
    'MAME SL XML files (*.xml)|*.xml', '');

  if not TestFilename(aFile) then Exit;
  
  aFolder := AskFolder('Select folder', '');
  if aFolder = '' then
  begin
    WriteLn('');
    WriteLn('');
    WriteLn('CANCELLED');
    WriteLn('---------');
    Exit;
  end;
  
  SLXMLFile := CreateStringList;
  WriteLn ('Reading: ' + aFile);  
  SLXMLFile.BeginUpdate;
  SLXMLFile.LoadFromFile(aFile);
  SLXMLFile.EndUpdate;
  SLXMLList := CreateStringList;
  
  // Simulating MAME -fulllist output in SLXMLList
  i := 0;
  while i < SLXMLFile.Count do
  begin
    aLine := SLXMLFile[i];
    
    if Pos('<software', aLine) > 1 then
    begin
      Inc(i);    
      // Joining software lines
      aPos := 1;
      while (i < SLXMLFile.Count) and (aPos > 0)do
      begin
        aName := SLXMLFile[i];
        aPos := Pos('</software', aName);
        aLine := aLine + aName;
        Inc(i);
      end;
      
      aID := Trim(ETKExtractBetween(aLine, 'name="', '"'));
      aName := Trim(ETKExtractBetween(aLine, '<description>', '</description>'));
      
      if aID <> '' then
      begin
        SLXMLList.Add(aID + '        "' + aName + '"');
      end;
    end
    else
      Inc(i);
  end;  
  SLXMLFile.Free;  
  SLXMLList.Sort;
  
  aFileList := CreateStringList;
  FindAllFiles(aFileList, aFolder, '', False); 
  aFileList.Sort;

  WriteLn('Folder: ' + aFolder);
  WriteLn('Renaming: ' + IntToStr(AFileList.Count) + ' files.)');

  while AFileList.Count > 0 do
  begin
    if (AFileList.Count and 511) = 511 then
      WriteLn('Renaming: ' + aFile +  '(' + IntToStr(AFileList.Count) + ' left.)');
      
    aFolder := ExtractFilePath(AFileList[0]);
    aFile := RemoveFromBrackets(ExtractFilenameOnly(AFileList[0]));
    aExt := ExtractFileExt(AFileList[0]);

    aPos := 1; // Enter the loop
    while (SLXMLList.Count > 0) and (aPos > 0) do
    begin
      aName := SLXMLList[0];
      aPos := Pos('"', aName);

      if aPos > 1 then
      begin
        // Extracting ID
        aID := Trim(Copy(aName, 1, aPos - 1));

        // Extracting Title (+ Version)
        aName := Trim(ETKCopyFrom(aName, aPos + 1)); // Removing first '"'
        aName := Trim(Copy(aName, 1, Length(aName) - 1)); // Removing last '"'

        aPos := CompareFileNames(aFile, aID);
        if aPos = 0 then // Match, rename file
        begin
          ETKFixTitle(aName, aID, outFile); // aID = SortTitle. It is useless.
          
//          WriteLn(aName + ' -> ' + aID + ' -> ' + outFile);
          
          if CompareFileNames(ExtractFilenameOnly(outFile), aFile) <> 0 then
          begin
            outFile := SetAsFolder(aFolder) + outFile + aExt;
            outFile := ETKCheckRenameFile(outFile);
            RenameFile(AFileList[0], outFile);
          end;
          
          //   aPos := 0; // Exit the loop <-- already aPos <= 0 
        end 
        else if aPos > 0 then // Not match, search next in SLXMLList and try again
        begin
          SLXMLList.Delete(0);
        end
        else // if aPos < 0 then // Not match, filename is not in SLXMLList
        begin
          //   aPos := 0; // Exit the loop <-- already aPos <= 0  
        end;  
      end;
      // else
      //   aPos := 0; // Exit the loop <-- already aPos <= 0 
    end;        
   
    // Next file
    AFileList.Delete(0);
  end;

  SLXMLList.Free;
  AFileList.Free; 
  WriteLn('');
  WriteLn('');
  WriteLn('DONE');
  WriteLn('----');   
  
end.
