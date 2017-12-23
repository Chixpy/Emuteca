{
[Info]
TODO: NOT WORKING, In development
With:
* mame -listclones > MAMEclones.txt

Moves images (or other media) from clones to parent. Useful for images of
How To's, Versus, Bosses, etc. wich the images valid for the parent are spreaded
between clones.
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
var
  ClonesFilename, ImagesFolder, aParent, aClone: string;
  ClonesList: TStringList;
  i, aPos: integer;
begin
  ClonesFilename := AskFile(
    'File with clones data (mame -listclones > MAMEclones.txt)',
    'All files (*.*)|*.*', 'MAMEclones.txt');
    
  if not FileExistsUTF8(ClonesFilename) then
  begin
    WriteLn('The file "' + ClonesFilename + '" not found.');
    exit;
  end;
  
  ImagesFolder := AskFolder('File with images or media', '');
  if not DirectoryExistsUTF8(ImagesFolder) then
  begin
    WriteLn('The folder "' + ImagesFolder + '" not found.');
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
      
      // TODO: Actually do it
      
      WriteLn(aClone + ' -> ' + aParent);
      
      Inc(i);
    end;
  finally
    ClonesList.Free
  end;
end.
