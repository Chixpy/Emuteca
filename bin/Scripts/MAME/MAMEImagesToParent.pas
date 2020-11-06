{
[Info]
TODO: NOT WORKING, In development
With:
* mame -listclones > MAMEclones.txt

Moves images (or other media) from clones to their parent. Files will be moved to a subfolder with parents name.

Useful for images of How To's, Versus, Bosses, etc. wich the images valid for the parent are spreaded between clones.

Files must have MAME ID's Name.
[Data]
Name=Chixpy
Version=0.01
Date=20171222
[Changes]
0.01
  + Initial working version
[EndInfo]
}
program MAMEImagesToParent;

function TestFilename(aFilename: string): boolean;
begin
  Result := FileExistsUTF8(aFilename);
  if not Result then
    WriteLn('The file "' + aFilename + '" was not found.');
end;

var
  FullFilename, ClonesFilename: string;
  ImagesFolder, aParent, aClone: string;
  FullList, ClonesList: TStringList;
  i, aPos: integer;
begin
  ClonesFilename := AskFile(
    'File with clones data (mame -listclones > MAMEclones.txt)',
    'All files (*.*)|*.*', 'MAMEclones.txt');
    
  if not TestFilename(ClonesFilename) then Exit;
  
  ImagesFolder := AskFolder('Folder with images or media', '');
  if not DirectoryExistsUTF8(ImagesFolder) then
  begin
    WriteLn('The folder "' + ImagesFolder + '" was not found.');
    exit;
  end;  
  
  ClonesList := CreateStringList;
  try
    ClonesList.LoadFromFile(ClonesFilename);
    ClonesList.Delete(0); // Removing header
    ClonesList.Sort; 
    
    i := 0;
    while i < ClonesList.Count do
    begin
      aParent := Trim(ClonesList[i]);
      aClone := Trim(Copy2SpaceDel(aParent));
      aParent := Trim(aParent);
      
      WriteLn(aClone + ' -> ' + aParent);

      ñññññ      
      
      
      Inc(i);
    end;
  finally
    ClonesList.Free
  end;
end.
