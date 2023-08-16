{
[Info]
Searchs duplicated files, with have same SHA1. And removes from system's
  software list.

It DOES NOT remove files from Hard Disk.
[Data]
Name=Chixpy
Version=0.01
Date=20230816
[Changes]
0.01 - 20230816
  + Initial version
[EndInfo]
}
program SearchDuplicates;

//uses uETKStrUtils;
{$I 'uETKStrUtils.pas'}

var
  DEBUG: Boolean;

function SelectDuplicate(aSys: cEmutecaSystem; FileList: TStringList): boolean;
var
  i, j: integer;
begin
  Result := True;
  if FileList.Count < 2 then Exit;

  // Adding option to keep all files and show the group
  // if the file is the same all file are in the same group
  //   unless some hand-made editing in current session
  FileList.Add('Keep all files (Group: ' +
    cEmutecaSoftware(FileList.Objects[0]).CachedGroup.Title +
    ')')
  
  j := AskOption('Duplicated files found', 
          'Which file do you want to KEEP in the list?', FileList);

  if j = -1 then
  begin
    Result := False;
    Exit;
  end;

  // Lastoption: Keep all files
  if j = (FileList.Count - 1) then
  begin
   Exit;
  end;

  WriteLn('');
  WriteLn(FileList[j]);

  i := 0;
  while i < (FileList.Count - 1) do
  begin
    if i <> j then
    begin
      WriteLn('    ' + FileList[i]);

      if not DEBUG then
        aSys.RemoveSoft(cEmutecaSoftware(FileList.Objects[i]))
    end;

    Inc(i);
  end;
end;

var
  aSystem: cEmutecaSystem;
  aSoftList: cEmutecaSoftList;
  aSoft: cEmutecaSoftware;
  aSL, slFiles: TStringList;
  aID: string;
  i, j, k: integer;
  Cont: Boolean;

begin
  i := AskYesNoCancel('Preview Changes', 'Do you want to preview changes only?');
  if i = mrCancel then
  begin
    WriteLn('¡Canceled!');
    Exit;
  end;
  DEBUG := i = mrYes;

  aSystem := ETKAskSystem(Emuteca.SystemManager.FullList);
   
  if not assigned(aSystem) then
  begin
    WriteLn('No system was selected.');
    Exit;
  end
  else
  begin
    WriteLn('');
    WriteLn('Selected System: ' + aSystem.Title);
  end;

  // Loading data of the system if it isn't loaded
  Emuteca.SystemManager.LoadSystemData(aSystem);
   
  aSoftList := aSystem.SoftManager.FullList;

  aSL := CreateStringList;
  aSL.Duplicates := dupAccept;
  aSL.CaseSensitive := False;

  WriteLn('');
  WriteLn('Sorting software by SHA1/CRC2.');
  i := 0;
  while i < aSoftList.Count do
  begin
    aSoft := aSoftList[i];
    aSL.AddObject(LowerCase(aSoft.SHA1), aSoft);
    Inc(i);
  end;
  
  aSL.Sort;

  WriteLn('');
  WriteLn('Searching duplicates.');

  slFiles := CreateStringList;
  slFiles.Duplicates := dupAccept;  
  Cont := True; // Continue
  aID := '';
  i := 0; 
  while (i < aSL.Count) and Cont do
  begin
    aSoft := cEmutecaSoftware(aSl.Objects[i]);

    if aSl[i] <> aID then
    begin
      Cont := SelectDuplicate(aSystem, slFiles);
    
      aID := aSl[i];
      slFiles.Clear;
    end;
    
    slFiles.AddObject(ExtractRelativepath(SetAsFolder(aSystem.BaseFolder),
      SetAsFolder(aSoft.Folder) + aSoft.FileName),
      aSoft);
    
    Inc(i);
  end;  
  
  // Testing last software
  if Cont then
    SelectDuplicate(aSystem, slFiles)
  else
  begin
    WriteLn('');
    WriteLn('¡Aborted!');
  end;


  aSL.Free;
  slFiles.Free;
  
  WriteLn('');
  WriteLn('---------');
  WriteLn('Finished!');
  WriteLn('---------');
end.
