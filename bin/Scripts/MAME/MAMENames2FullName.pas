{ Emuteca Script
[Info]
This script renames files from a folder with MAME names to full game names:
* Example: sf2.<ext> -> Street Fighter II -  The World Warrior (World 910522).<ext>

Parenthesis are not removed, to do it use Misc/RemoveParenthesis.pas

Usefull for MAME image sets, as Emuteca DB by default uses game titles for searching media.

It needs a txt file created with:
* mame -listfull > MAMEfull.txt
[Data]
Name=Chixpy
Version=0.02
Date=20201108
[Changes]
0.02 20201108
  * Renaming files to SortTitle, instead actual title.
0.01 20201105
  + Initial working version
[EndInfo]
}
program MAMENames2FullName;

//uses uETKStrUtils, uETKFileUtils;
{$I '../Units/uETKStrUtils.pas'}
{$I '../Units/uETKFileUtils.pas'}

var
  MAMEFull, aFolder, aFile, aExt, outFile, aID, aName: string;
  FullList, aFileList: TStringList;
  i, aPos: integer;
  
begin
  MAMEFull := AskFile(
    'File with full data (mame -listfull > MAMEfull.txt)',
    'All files (*.*)|*.*', '');

  if not TestFilename(MAMEFull) then Exit;
  
  aFolder := AskFolder('Select folder', '');
  if aFolder = '' then
  begin
    WriteLn('');
    WriteLn('');
    WriteLn('CANCELLED');
    WriteLn('---------');
    Exit;
  end;
  
  FullList := CreateStringList;
  WriteLn ('Reading: ' + MAMEFull);
  FullList.BeginUpdate;
  FullList.LoadFromFile(MAMEFull);
  FullList.Delete(0); // Removing header
  FullList.Sort; // Sorting for faster search
  FullList.EndUpdate;
  
  aFileList := CreateStringList;
  FindAllFiles(aFileList, aFolder, '', False); 
  aFileList.Sort;
  
  WriteLn('Renaming: ' + IntToStr(AFileList.Count) + ' files.)');

  while AFileList.Count > 0 do
  begin
    if (AFileList.Count and 511) = 511 then
      WriteLn('Renaming: ' + aFile +  '(' + IntToStr(AFileList.Count) + ' left.)');

    aFolder := ExtractFilePath(AFileList[0]);
    aFile := RemoveFromBrackets(ExtractFilenameOnly(AFileList[0]));
    aExt := ExtractFileExt(AFileList[0]);

    aPos := 1; // Enter the loop
    while (FullList.Count > 0) and (aPos > 0) do
    begin
      aName := FullList[0];
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
          aID := '';
          ETKFixTitle(aName, aID, outFile); // aID = SortTitle. It is useless.
          
//          WriteLn(aName + ' -> ' + aID + ' -> ' + outFile);
          
          if CompareFileNames(ExtractFilenameOnly(outFile), aFile) <> 0 then
          begin
            outFile := SetAsFolder(aFolder) + outFile + aExt;
            outFile := ETKCheckRenameFile(outFile);
            RenameFile(AFileList[0], outFile);
          end;
          
          // aPos := 0; // Exit the loop <-- already aPos <= 0 
        end 
        else if aPos > 0 then // Not match, search next in FullList and try again
        begin
            FullList.Delete(0);
        end
        else // if aPos < 0 then // Not match, filename is not in FullList
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

  AFileList.Free;
  FullList.Free;  
  WriteLn('');
  WriteLn('');
  WriteLn('DONE');
  WriteLn('----');     
end.
