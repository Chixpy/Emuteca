unit ufLEmuTKMediaManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, VirtualTrees, Forms, Controls,
  Graphics, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, ActnList, LCLType, Buttons, LazFileUtils,
  LazUTF8, uCHXStrUtils, uCHXImageUtils, uCHXFileUtils, ufCHXFrame, ufCHXForm,
  ufCHXProgressBar, uEmutecaCommon, ucEmuteca, ucEmutecaSystem, ucEmutecaGroup,
  ucEmutecaSoftware, ufEmutecaSystemCBX, uGUIConfig, uLEmuTKCommon;

type

  { TfmLEmuTKMediaManager }

  TfmLEmuTKMediaManager = class(TfmCHXFrame)
    actChangeFileName: TAction;
    actDeleteFile: TAction;
    ActionList: TActionList;
    bRename: TBitBtn;
    chkCopyFile: TCheckBox;
    chkSimilarFiles: TCheckBox;
    gbxImages: TGroupBox;
    gbxMusic: TGroupBox;
    gbxOtherFiles: TGroupBox;
    gbxRename: TGroupBox;
    gbxSource: TGroupBox;
    gbxSystem: TGroupBox;
    gbxTarget: TGroupBox;
    gbxTexts: TGroupBox;
    gbxVideos: TGroupBox;
    ilActions: TImageList;
    lbxImages: TListBox;
    lbxMusic: TListBox;
    lbxOtherFiles: TListBox;
    lbxTexts: TListBox;
    lbxVideos: TListBox;
    pCenter: TPanel;
    pcSource: TPageControl;
    pcTarget: TPageControl;
    pLeft: TScrollBox;
    pRight: TPanel;
    pSimilar: TPanel;
    sbSource: TStatusBar;
    sbTarget: TStatusBar;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    pagAllFiles: TTabSheet;
    pagFilesWOGroup: TTabSheet;
    pagFilesWOSoft: TTabSheet;
    pagFilesOtherFolder: TTabSheet;
    pagAllGroups: TTabSheet;
    pagGroupsWOFile: TTabSheet;
    pagAllSoft: TTabSheet;
    pagSoftWOFile: TTabSheet;
    pagOtherFiles: TTabSheet;
    tbSimilarThresold: TTrackBar;
    vstAllFiles: TVirtualStringTree;
    vstFilesWOGroup: TVirtualStringTree;
    vstFilesWOSoft: TVirtualStringTree;
    vstAllGroups: TVirtualStringTree;
    vstOtherFiles: TVirtualStringTree;
    vstFilesOtherFolder: TVirtualStringTree;
    vstGroupsWOFile: TVirtualStringTree;
    vstAllSoft: TVirtualStringTree;
    vstSoftWOFile: TVirtualStringTree;
    procedure actChangeFileNameExecute(Sender: TObject);
    procedure chkSimilarFilesChange(Sender: TObject);
    procedure lbxFolderSelectionChange(Sender: TObject; User: boolean);
    procedure tbSimilarThresoldChange(Sender: TObject);
    procedure vstFileChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstFileCompareNodes(Sender: TBaseVirtualTree;
      Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);
    procedure vstFileKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure vstGroupChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstGroupCompareNodes(Sender: TBaseVirtualTree;
      Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);
    procedure vstGroupKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure vstFileFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstFileGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure vstAllGroupsInitNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure vstKeyPress(Sender: TObject; var Key: char);
    procedure vstSoftChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstSoftCompareNodes(Sender: TBaseVirtualTree;
      Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);
    procedure vstSoftGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure vstGroupGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);

  private
    FCurrentGroup: cEmutecaGroup;
    FCurrSystem: cEmutecaSystem;
    FEmuteca: cEmuteca;
    FExtFilter: TStrings;
    FfmSystemCBX: TfmEmutecaSystemCBX;
    FGUIConfig: cGUIConfig;
    FSHA1Folder: string;
    FSourceFile: string;
    FSourceFolder: string;
    FTargetFile: string;
    FTargetFolder: string;
    procedure SetCurrentGroup(AValue: cEmutecaGroup);
    procedure SetCurrSystem(AValue: cEmutecaSystem);
    procedure SetEmuteca(AValue: cEmuteca);
    procedure SetExtFilter(AValue: TStrings);
    procedure SetGUIConfig(AValue: cGUIConfig);
    procedure SetSHA1Folder(AValue: string);
    procedure SetSourceFile(AValue: string);
    procedure SetSourceFolder(AValue: string);
    procedure SetTargetFile(AValue: string);
    procedure SetTargetFolder(AValue: string);

  protected
    property fmSystemCBX: TfmEmutecaSystemCBX read FfmSystemCBX;

    property CurrSystem: cEmutecaSystem read FCurrSystem write SetCurrSystem;

    property SourceFile: string read FSourceFile write SetSourceFile;
    {< Name of the source file. }
    property SourceFolder: string read FSourceFolder write SetSourceFolder;
    {< Folder of the source file. }
    property TargetFile: string read FTargetFile write SetTargetFile;
    {< Name of the target file. }
    property TargetFolder: string read FTargetFolder write SetTargetFolder;
    {< Folder of the target file. }

    property CurrentGroup: cEmutecaGroup read FCurrentGroup
      write SetCurrentGroup;


    property ExtFilter: TStrings read FExtFilter write SetExtFilter;
    {< Extensions of the current selected Media. }

    procedure UpdateStatusBars;
    procedure UpdateVST(aFolder: string);
 {< Update the Virtual String Trees.
   @param(aFolder Folder where search the media.)
 }



    function SelectSystem(aSystem: cEmutecaSystem): boolean;
    {< Selects a system.
    }
    procedure LoadSysFolders;
    {< Load CurrSystem folders in lbx. }

    // File lists
    // ----------

    function CurrentFileList: TCustomVirtualStringTree;
    {< Returns the current file list shown.
    }

    procedure FilterFiles;
    {< Show only files with similar name to current selected game or group.
    }

    function AddFile(aFolder: string; Info: TSearchRec): boolean; overload;
 {< Adds a file to the lists.
   For use with IterateFolder.
   @param(aFolder Folder where the file is in.)
   @param(Info TSearchRec with file data.)
   @return(Always @true; needed for IterateFolder.)
 }
    function AddFile(aFolder, aName: string): boolean; overload;
 {< Adds a file to the lists.
   For manual use @(and hacky updates@).
   @param(aFolder Folder where the file is in.)
   @param(aName Name of the file.)
   @return(Always @true @(useless until a reason to stop batch operations
     will be found.@).)
 }

   {

 property MediaFiles: TStringList read FMediaFiles write SetMediaFiles;
 //< Mediafiles assigned to the current game or group.
 property CurrentMediaIndex: integer
   read FCurrentMediaIndex write SetCurrentMediaIndex;
 //< Index of the current media file.

 // TODO 3: Maybe this 4 methods can be reduced to 2 without ofuscate them...


 function AddFilesOtherFolder(aFolder: string;
   Info: TSearchRec): boolean; overload;
 {< Add files or folders to vstFilesOtherFolder.
   @param(aFolder Folder where the file is in.)
   @param(Info TSearchRec with folder or file data.)
   @return(Always @true; needed for IterateFolder.)
 }


 procedure UpdateFileOtherFolder(const afolder: string);

 procedure ChangeGroupMedia(aGroup: cEmutecaGameGroup);
 {< Change the media preview to the group media.
   @param(aGroup The game group with it's media will be previewed.)
 }
 procedure ChangeGameMedia(aGame: cEmutecaGame);
 {< Change the media preview to the game media.
   @param(aGame The game with it's media will be previewed.)
 }
 procedure ChangeFileMedia(aFolder, aName: string);
 {< Change the media preview to the file.
   @param(aFolder Folder were the file is.)
   @param(aName Name of the file.)
 }
 procedure ClearMedia;
 {< Clear media preview fields.
 }
 procedure NextMedia;
 {< Change to next media file found.
 }
 procedure PreviousMedia;
 {< Change to previous media file found.
 }
 procedure ShowMedia;
 {< Update current media preview. }
 procedure OpenImagesInViewer;
 {< Open the current previewed image in Image Viewer. }

 procedure ChangeFileName;
 {< Change the name of the current selected file, to one used by the
   selected game o group.
 }
 procedure DeleteFile;
 {< Delete the current selected (source) file FROM DISC PHYSICALLY. }
 procedure DeleteAllFiles;
 {< Delete all VISIBLE (i.e. no hidden) files from the current list
      FROM DISC PHYSICALLY. }
 procedure MoveFile;
 {< Move current selected (source) file to another folder. }
 procedure MoveAllFiles;
 {< Move all VISIBLE (i.e. no hidden) files from the current list to
     another folder. }

 procedure RemoveFileVSTFiles(aFolder, aFile: string);
 {< Remove a file from lists (not physically).
   Used for hacky updates.
   @param(aFolder Folder were the file is.)
   @param(aFile Name of the file.)
 }
 procedure RemoveGroupWOFile(aFile: string);
 {< Remove groups from vstGroupsWOFile list that uses aFile.
   Used for hacky updates.
   @param(aFile Name of file that maybe is used by groups.)
 }

}




    procedure ClearFrameData; override;
    procedure LoadFrameData; override;

    procedure SetGUIIconsIni(AValue: string); override;

  public
    property Emuteca: cEmuteca read FEmuteca write SetEmuteca;
    property SHA1Folder: string read FSHA1Folder write SetSHA1Folder;
    property GUIConfig: cGUIConfig read FGUIConfig write SetGUIConfig;

    // Creates a form with Media Manager.
    class function SimpleForm(aEmuteca: cEmuteca; aGUIIconsIni: string;
      aGUIConfig: cGUIConfig): integer;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmLEmuTKMediaManager }

procedure TfmLEmuTKMediaManager.SetSHA1Folder(AValue: string);
begin
  FSHA1Folder := SetAsFolder(AValue);
end;

procedure TfmLEmuTKMediaManager.SetSourceFile(AValue: string);
begin
  FSourceFile := SetAsFile(AValue);

  // We want to update the TargetFile extension.
  TargetFile := FTargetFile;

  // UpdateStatusBars; Updateted by SetTargetFile
end;

procedure TfmLEmuTKMediaManager.SetSourceFolder(AValue: string);
begin
  FSourceFolder := SetAsFolder(AValue);
  UpdateStatusBars;
end;

procedure TfmLEmuTKMediaManager.SetTargetFile(AValue: string);
begin
  FTargetFile := RemoveFromBrackets(ExtractFileNameOnly(AValue));

  if (FTargetFile <> '') and (SourceFile <> '') then
    FTargetFile := FTargetFile + UTF8LowerCase(ExtractFileExt(SourceFile));

  UpdateStatusBars;
end;

procedure TfmLEmuTKMediaManager.SetTargetFolder(AValue: string);
begin
  FTargetFolder := SetAsFolder(AValue);
  UpdateStatusBars;
end;

procedure TfmLEmuTKMediaManager.UpdateStatusBars;
begin
  sbSource.SimpleText := 'Source: ' + SourceFolder + SourceFile;
  sbTarget.SimpleText := 'Target: ' + TargetFolder + TargetFile;
end;

procedure TfmLEmuTKMediaManager.UpdateVST(aFolder: string);
var
  pFilename: ^string;
  pGroup: ^cEmutecaGroup;
  pSoft: ^cEmutecaSoftware;
  aFileName: string;
  aGroup: cEmutecaGroup;
  aSoft: cEmutecaSoftware;
  SoftNode, GroupNode, FileNode: PVirtualNode;
  FileComp: Integer;
begin
  SourceFile := '';
  SourceFolder := '';
  vstAllFiles.Clear;
  vstOtherFiles.Clear;
  vstFilesWOGroup.Clear;
  vstFilesWOSoft.Clear;
  vstGroupsWOFile.Clear;
  vstSoftWOFile.Clear;

  aFolder := SetAsFolder(aFolder);
  if not DirectoryExistsUTF8(aFolder) then
    Exit;

  TargetFolder := aFolder;
  SourceFolder := aFolder;
  SourceFile := '';

  // Adding all files/folders of the target folder
  vstAllFiles.BeginUpdate;
  vstOtherFiles.BeginUpdate;
  frmCHXProgressBar.UpdTextAndBar('Searching files...',
    'This can take some time', '', 1, 4);
  IterateFolderObj(aFolder, @AddFile, False);
  vstAllFiles.EndUpdate;
  vstOtherFiles.EndUpdate;

  // vstAllFiles -> vstFilesWOGroup and
  //   vstAllGroups -> vstGroupsWOFile.
  frmCHXProgressBar.UpdTextAndBar('Searching...',
    'Files without group and groups without file', '', 2, 4);

  // Sorting vstAllFiles and vstvstAllGroups to iterate them;
  vstAllFiles.SortTree(0, VirtualTrees.sdAscending, True); // By filename
  vstAllGroups.SortTree(1, VirtualTrees.sdAscending, True); // By filename

  vstFilesWOGroup.BeginUpdate;
  vstGroupsWOFile.BeginUpdate;
  GroupNode := vstAllGroups.GetFirstChild(nil);
  FileNode := vstAllFiles.GetFirstChild(nil);
  while (assigned(GroupNode)) and (assigned(FileNode)) do
  begin
    if not assigned(GroupNode) then
    begin
      aGroup := nil;
      pFilename := vstAllFiles.GetNodeData(FileNode);
      aFileName := string(pFilename^);
      FileComp := -1;
    end
    else if not assigned(FileNode) then
    begin
      pGroup := vstAllGroups.GetNodeData(GroupNode);
      aGroup := pGroup^;
      aFileName := '';
      FileComp := 1;
    end
    else
    begin
      pFilename := vstAllFiles.GetNodeData(FileNode);
      aFileName := string(pFilename^);
      pGroup := vstAllGroups.GetNodeData(GroupNode);
      aGroup := pGroup^;
      FileComp := CompareFilenames(ExtractFileNameOnly(aFileName), aGroup.ID);
    end;


    if FileComp = 0 then
    begin  // Match!!
      FileNode := vstAllFiles.GetNextSibling(FileNode);
      GroupNode := vstAllGroups.GetNextSibling(GroupNode);
    end
    else if FileComp < 0 then
    begin // Not match, File is behind Group
      pFilename := vstFilesWOGroup.GetNodeData(
        vstFilesWOGroup.AddChild(nil));
      pFilename^ := aFileName;
      FileNode := vstAllFiles.GetNextSibling(FileNode);
    end
    else  // Not match, Group is behind Group File
    begin
      pGroup := vstGroupsWOFile.GetNodeData(
        vstGroupsWOFile.AddChild(nil));
      pGroup^ := aGroup;
      GroupNode := vstAllGroups.GetNextSibling(GroupNode);
    end;
  end;

  vstFilesWOGroup.EndUpdate;
  vstGroupsWOFile.EndUpdate;




  //    // Searching for files used by the groups and groups without file
  //    StatusBar.SimpleText := rsfmmSearchFilesWOGroup;
  //    Continue := True;
  //    i := 0;
  //    j := GameManager.GroupCount;
  //    Application.ProcessMessages;
  //    vstFilesWOGroup.BeginUpdate;
  //    vstGroupsWOFile.BeginUpdate;

  //    GroupNode := vstAllGroups.GetFirstChild(nil);
  //    while (GroupNode <> nil) and (Continue) do
  //    begin
  //      pGroup := vstAllGroups.GetNodeData(GroupNode);
  //      if frmProgress <> nil then
  //        Continue := frmProgress.UpdTextAndBar(rsfmmSearchFilesWOGroup,
  //          pGroup^.Name, '', i, j);

  //      FileNode := vstFilesWOGroup.GetFirstChild(nil);
  //      Found := False;
  //      while (FileNode <> nil) and (not Found) do
  //      begin
  //        pFilename := vstFilesWOGroup.GetNodeData(FileNode);
  //        aFileName := ChangeFileExt(pFilename^, kEmutecaVirtualGroupExt);
  //        // TODO 1: LINUX
  //        if UTF8CompareText(aFileName, pGroup^.MediaFileName) = 0 then
  //          Found := True
  //        else
  //          FileNode := vstFilesWOGroup.GetNextSibling(FileNode);
  //      end;

  //      if Found then
  //        vstFilesWOGroup.DeleteNode(FileNode)
  //      else
  //      begin
  //        if not GameManager.GroupMediaExists(aFolder, pGroup^, ExtFilter) then
  //        begin
  //          PGameGroup2 := vstGroupsWOFile.GetNodeData(
  //            vstGroupsWOFile.AddChild(nil));
  //          PGameGroup2^ := pGroup^;
  //        end;
  //      end;
  //      Inc(i);
  //      GroupNode := vstAllGroups.GetNextSibling(GroupNode);
  //    end;
  //    vstGroupsWOFile.EndUpdate;
  //    vstFilesWOGroup.EndUpdate;
  //    vstGroupsWOFile.SortTree(0, sdAscending, True);
  //    vstFilesWOGroup.SortTree(0, sdAscending, True);

  //    // Searching files not used by games
  //    StatusBar.SimpleText := rsfmmSearchFilesWOGame;
  //    Application.ProcessMessages;
  //    Continue := True;
  //    i := 0;
  //    j := vstFilesWOGroup.ChildCount[nil];
  //    if frmProgress <> nil then
  //      frmProgress.Show;
  //    vstFilesWOGame.Clear;
  //    vstFilesWOGame.BeginUpdate;
  //    GroupNode := vstFilesWOGroup.GetFirstChild(nil);
  //    while (GroupNode <> nil) and (Continue) do
  //    begin
  //      Found := False;
  //      pFilename := vstFilesWOGroup.GetNodeData(GroupNode);
  //      aFileName := ExtractFileNameOnly(pFilename^);
  //      Continue := frmProgress.UpdTextAndBar(rsfmmSearchFilesWOGame,
  //        aFileName, '', i, j);
  //      FileNode := vstAllGames.GetFirstChild(nil);
  //      while not Found and (FileNode <> nil) do
  //      begin
  //        pSoft := vstAllGames.GetNodeData(FileNode);
  //        if SameFileName(aFileName, RemoveFromBrackets(pSoft^.FileName)) then
  //          Found := True;
  //        FileNode := vstAllGames.GetNextSibling(FileNode);
  //      end;

  //      if not Found then
  //      begin
  //        pFilename := vstFilesWOGame.GetNodeData(vstFilesWOGame.AddChild(nil));
  //        pFilename^ := string(vstFilesWOGroup.GetNodeData(GroupNode)^);
  //      end;

  //      GroupNode := vstFilesWOGroup.GetNextSibling(GroupNode);
  //      Inc(i);
  //    end;
  //    vstFilesWOGame.EndUpdate;
  //    vstFilesWOGame.SortTree(0, sdAscending, True);

  //  finally
  //    Self.Enabled:= True;
  //    FreeAndNil(frmProgress);
  //  end;

  //  StatusBar.SimpleText := '';
  frmCHXProgressBar.UpdateProgressBar(0, 0);

end;

function TfmLEmuTKMediaManager.SelectSystem(aSystem: cEmutecaSystem): boolean;
begin
  Result := True;
  CurrSystem := aSystem;
end;

procedure TfmLEmuTKMediaManager.FilterFiles;
var
  FileNode: PVirtualNode;
  aTargetFile: string;
  pFileName: ^string;
  aVST: TCustomVirtualStringTree;
begin
  aVST := CurrentFileList;
  if aVST = nil then
    Exit;

  aTargetFile := Trim(ExtractFileNameOnly(TargetFile));
  aVST.BeginUpdate;
  FileNode := aVST.GetFirstChild(nil);
  while assigned(FileNode) do
  begin
    if chkSimilarFiles.Checked and (aTargetFile <> '') then
    begin
      pFileName := aVST.GetNodeData(FileNode);

      aVST.IsVisible[FileNode] :=
        TextSimilarity(aTargetFile, pFileName^) >= tbSimilarThresold.Position;
    end
    else
    begin
      aVST.IsVisible[FileNode] := True;
    end;

    FileNode := aVST.GetNextSibling(FileNode);
  end;
  aVST.EndUpdate;
end;

function TfmLEmuTKMediaManager.AddFile(aFolder: string;
  Info: TSearchRec): boolean;
begin
  Result := True;
  if (Info.Attr and faDirectory) <> 0 then
  begin
    if (Info.Name = '.') or (Info.Name = '..') then
      Exit;
    Result := AddFile(aFolder, Info.Name + krsVirtualFolderExt);
  end
  else
    Result := AddFile(aFolder, Info.Name);
end;

function TfmLEmuTKMediaManager.AddFile(aFolder, aName: string): boolean;
var
  pFile: ^string;
begin
  Result := True;

  if SupportedExtCT(aName, krsVirtualFolderExt) then
  begin
    // It's a folder

    // TOD0 2: Test if it's empty or not have the current type of mediafiles
    pFile := vstAllFiles.GetNodeData(vstAllFiles.AddChild(nil));
    // See: TfrmMediaManager.vstAllFilesFreeNode;
    pFile^ := aName;
  end
  else
  begin // Look if it is a compressed archive or current type of mediafiles -
    if SupportedExtSL(aName, ExtFilter) or
      SupportedExtSL(aName, Emuteca.Config.CompressedExtensions) then
      pFile := vstAllFiles.GetNodeData(vstAllFiles.AddChild(nil))
    else
      pFile := vstOtherFiles.GetNodeData(vstOtherFiles.AddChild(nil));
    // See: TfrmMediaManager.vstAllFilesFreeNode;
    pFile^ := aName;
  end;
end;

procedure TfmLEmuTKMediaManager.ClearFrameData;
begin

end;

procedure TfmLEmuTKMediaManager.LoadFrameData;
begin
  Enabled := assigned(Emuteca);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

end;

procedure TfmLEmuTKMediaManager.SetGUIIconsIni(AValue: string);
begin
  inherited SetGUIIconsIni(AValue);

  ReadActionsIcons(GUIIconsIni, Self.Name, ilActions, ActionList);
end;

procedure TfmLEmuTKMediaManager.LoadSysFolders;
var
  i: integer;
  pSoft: ^cEmutecaSoftware;
  aSoft: cEmutecaSoftware;
begin
  lbxImages.Clear;
  lbxTexts.Clear;
  lbxMusic.Clear;
  lbxVideos.Clear;

  vstAllGroups.Clear;
  vstAllSoft.Clear;

  SourceFile := '';
  SourceFolder := '';
  TargetFile := '';
  TargetFolder := '';

  if not Assigned(CurrSystem) then
    Exit;

  lbxImages.Clear;
  lbxImages.Items.Add('Icons');
  lbxImages.Items.AddStrings(CurrSystem.ImageCaptions, False);
  lbxTexts.Items.Assign(CurrSystem.TextCaptions);
  lbxMusic.Items.Assign(CurrSystem.MusicCaptions);
  lbxVideos.Items.Assign(CurrSystem.VideoCaptions);

  vstAllGroups.RootNodeCount := CurrSystem.GroupManager.VisibleList.Count;

  // Adding only Soft with different filename for its group.
  vstAllSoft.BeginUpdate;
  i := 0;
  while i < CurrSystem.SoftManager.FullList.Count do
  begin
    aSoft := CurrSystem.SoftManager.FullList[i];
    if not aSoft.MatchGroupFile then
    begin
      pSoft := vstAllSoft.GetNodeData(vstAllSoft.AddChild(nil));
      pSoft^ := aSoft;
    end;
    Inc(i);
  end;
  vstAllSoft.EndUpdate;
end;

function TfmLEmuTKMediaManager.CurrentFileList: TCustomVirtualStringTree;
begin
  case pcSource.ActivePageIndex of
    0: Result := vstAllFiles;
    1: Result := vstOtherFiles;
    2: Result := vstFilesWOGroup;
    3: Result := vstFilesWOSoft;
    4: Result := vstFilesOtherFolder;
    else
      Result := nil;
  end;
end;

procedure TfmLEmuTKMediaManager.lbxFolderSelectionChange(Sender: TObject;
  User: boolean);
var
  aLBX: TListBox;
begin
  if not User then
    Exit;

  aLBX := TListBox(Sender);

  if not assigned(aLBX) then
    Exit;

  if aLBX <> lbxImages then
    lbxImages.ItemIndex := -1;
  if aLBX <> lbxTexts then
    lbxTexts.ItemIndex := -1;
  if aLBX <> lbxMusic then
    lbxMusic.ItemIndex := -1;
  if aLBX <> lbxVideos then
    lbxVideos.ItemIndex := -1;
  if aLBX <> lbxOtherFiles then
    lbxOtherFiles.ItemIndex := -1;

  if aLBX.ItemIndex = -1 then
    Exit;

  ExtFilter := nil;

  if aLBX = lbxImages then
  begin
    case aLBX.ItemIndex of
      0: UpdateVST(CurrSystem.IconFolder);
      else
        UpdateVST(CurrSystem.ImageFolders[aLBX.ItemIndex - 1]);
    end;
    if Assigned(GUIConfig) then
      ExtFilter := GUIConfig.ImageExtensions;
  end
  else if aLBX = lbxTexts then
  begin
    UpdateVST(CurrSystem.TextFolders[aLBX.ItemIndex]);
    if Assigned(GUIConfig) then
      ExtFilter := GUIConfig.TextExtensions;
  end
  else if aLBX = lbxMusic then
  begin
    UpdateVST(CurrSystem.MusicFolders[aLBX.ItemIndex]);
    //    if Assigned(GUIConfig) then
    // ExtFilter := GUIConfig.MusicExtensions;
  end
  else if aLBX = lbxVideos then
  begin
    UpdateVST(CurrSystem.VideoFolders[aLBX.ItemIndex]);
    //    if Assigned(GUIConfig) then
    // ExtFilter := GUIConfig.VideoExtensions;
  end
  else if aLBX = lbxOtherFiles then
  begin
    UpdateVST('');
    // lbxOtherFiles.ItemIndex := -1;
  end
  else
    UpdateVST('');
end;

procedure TfmLEmuTKMediaManager.tbSimilarThresoldChange(Sender: TObject);
begin
  if chkSimilarFiles.Checked then
    FilterFiles;
end;

procedure TfmLEmuTKMediaManager.vstFileChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  if Node = nil then
  begin
    SourceFile := '';
    Exit;
  end;
  SourceFolder := TargetFolder;
  SourceFile := string(Sender.GetNodeData(Node)^);
end;

procedure TfmLEmuTKMediaManager.vstFileCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);
var
  pFile1, pFile2: ^string;
begin
  Result := 0;
  pFile1 := Sender.GetNodeData(Node1);
  pFile2 := Sender.GetNodeData(Node2);

  case Column of
    -1, 0: Result := CompareFilenames(pFile1^, pFile2^);
  end;
end;

procedure TfmLEmuTKMediaManager.vstFileKeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin
  if Shift = [] then
  begin
    case Key of
      VK_RETURN: actChangeFileName.Execute;
      VK_DELETE: actDeleteFile.Execute;
    end;
  end;
end;

procedure TfmLEmuTKMediaManager.vstGroupChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  PData: ^cEmutecaGroup;
begin
  if Node = nil then
  begin
    TargetFile := '';
    Exit;
  end;

  PData := Sender.GetNodeData(Node);
  CurrentGroup := PData^;
  TargetFile := CurrentGroup.ID + krsVirtualExt;

  FilterFiles;
end;

procedure TfmLEmuTKMediaManager.vstGroupCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);
var
  pGroup1, pGroup2: ^cEmutecaGroup;
begin
  Result := 0;
  pGroup1 := Sender.GetNodeData(Node1);
  pGroup2 := Sender.GetNodeData(Node2);

  case Column of
    -1, 0: Result := UTF8CompareText(pGroup1^.Title, pGroup2^.Title);
    1: Result := CompareFilenames(pGroup1^.ID, pGroup2^.ID);
  end;
end;

procedure TfmLEmuTKMediaManager.vstGroupKeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin
  if Shift = [] then
  begin
    case Key of
      VK_RETURN: actChangeFileName.Execute;
    end;
  end;
end;

procedure TfmLEmuTKMediaManager.chkSimilarFilesChange(Sender: TObject);
begin
  FilterFiles;
end;

procedure TfmLEmuTKMediaManager.actChangeFileNameExecute(Sender: TObject);
var
  TargetPath: string;
  SourcePath: string;
  IsFolder: boolean;
  aBool: boolean;
begin
  if (SourceFolder = PathDelim) or (SourceFolder = '') then
    Exit;
  if (TargetFolder = PathDelim) or (TargetFolder = '') then
    Exit;
  if SourceFile = '' then
    Exit;
  if TargetFile = '' then
    Exit;

  TargetPath := TargetFolder + TargetFile;
  SourcePath := SourceFolder + SourceFile;

  if TargetPath = SourcePath then
    Exit;

  // Testing if it's a folder
  IsFolder := CompareFileExt(SourceFile, krsVirtualFolderExt) = 0;
  if IsFolder then
  begin // Removing virtual extension of folders
    TargetPath := ExtractFileNameWithoutExt(TargetPath);
    SourcePath := ExtractFileNameWithoutExt(SourcePath);
  end;


  // Checking source existence, may be it's redundant...
  if IsFolder then
    aBool := DirectoryExistsUTF8(SourcePath)
  else
    aBool := FileExistsUTF8(SourcePath);
  if not aBool then
    Exit;

  // Checking target existence for files
  if IsFolder then
    aBool := DirectoryExistsUTF8(TargetPath)
  else
    aBool := FileExistsUTF8(TargetPath);
  if aBool then
  begin
    if MessageDlg(Format('%0:s already exists. ¿Overwrite?', [TargetPath]),
      mtConfirmation, [mbYes, mbNo], -1) = mrNo then
      // TODO 2: Merge folders?
      Exit
    else
    begin
      if IsFolder then
        DeleteDirectory(UTF8ToSys(TargetPath), False)
      else
        DeleteFileUTF8(TargetPath);
    end;
  end;

  // Copy or rename the file
  if chkCopyFile.Checked then
  begin
    // TODO 5: HACK: Where is CopyFileUTF8?
    // TODO 4: How can I copy folders?
    if not IsFolder then
      CopyFile(UTF8ToSys(SourcePath), UTF8ToSys(TargetPath),
        [cffOverwriteFile, cffCreateDestDirectory], True)
    else
    begin
      CopyDirTree(UTF8ToSys(SourcePath), UTF8ToSys(TargetPath),
        [cffOverwriteFile, cffCreateDestDirectory]);
    end;
  end
  else
  begin
    RenameFileUTF8(SourcePath, TargetPath);
    // Removing Source file from ALL vstFiles
    //RemoveFileVSTFiles(SourceFolder, SourceFile);
  end;


  //// Quick update of VST lists
  //// -------------------------

  //// Removing Target file from vstGroupWOFile.
  //RemoveGroupWOFile(TargetFile);

  // Adding TargetFile.
  AddFile(TargetFolder, TargetFile);

  SourceFile := '';

  // Clear TargetFile only if pagGroupsWOFile is active because
  //  the group is deleted from its list.
  if CurrentFileList = vstGroupsWOFile then
  begin
    TargetFile := '';
    //  ClearMedia;
  end;

  FilterFiles;
end;

procedure TfmLEmuTKMediaManager.vstFileFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  pFile: ^string;
begin
  pFile := Sender.GetNodeData(Node);
  Finalize(pFile^);
end;

procedure TfmLEmuTKMediaManager.vstFileGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  pFile: ^string;
begin
  pFile := Sender.GetNodeData(Node);
  if pFile^ = '' then
    Exit;
  case TextType of
    ttNormal: case Column of
        -1, 0: CellText := pFile^;
      end;
    ttStatic: ;
  end;
end;

procedure TfmLEmuTKMediaManager.vstAllGroupsInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  pGroup: ^cEmutecaGroup;
begin
  if not assigned(CurrSystem) then
    Exit;
  pGroup := Sender.GetNodeData(Node);
  pGroup^ := CurrSystem.GroupManager.VisibleList[Node^.Index];
end;

procedure TfmLEmuTKMediaManager.vstKeyPress(Sender: TObject; var Key: char);
begin
  // Removing "Beep" sound when key Return is pressed
  case Key of
    #13, #10: Key := #0;
  end;
end;

procedure TfmLEmuTKMediaManager.vstSoftChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  pSoft: ^cEmutecaSoftware;
begin
  if Node = nil then
  begin
    TargetFile := '';
    Exit;
  end;
  pSoft := Sender.GetNodeData(Node);
  TargetFile := pSoft^.FileName;
  CurrentGroup := cEmutecaGroup(pSoft^.CachedGroup);
  FilterFiles;
end;

procedure TfmLEmuTKMediaManager.vstSoftCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: integer);
var
  pSoft1, pSoft2: ^cEmutecaSoftware;
begin
  Result := 0;
  pSoft1 := Sender.GetNodeData(Node1);
  pSoft2 := Sender.GetNodeData(Node2);

  case Column of
    -1, 0: Result := UTF8CompareText(pSoft1^.Title, pSoft2^.Title);
    1: Result := UTF8CompareText(pSoft1^.CachedGroup.Title,
        pSoft2^.CachedGroup.Title);
    2: Result := CompareFilenames(pSoft1^.FileName, pSoft2^.FileName);
  end;
end;

procedure TfmLEmuTKMediaManager.vstSoftGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  pSoft: ^cEmutecaSoftware;
begin
  pSoft := Sender.GetNodeData(Node);
  if pSoft^ = nil then
    Exit;
  case TextType of
    ttNormal: case Column of
        -1, 0: CellText := pSoft^.Title;
        1: CellText := pSoft^.CachedGroup.Title;
        2: CellText := pSoft^.FileName;
      end;
  end;
end;

procedure TfmLEmuTKMediaManager.vstGroupGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  pGroup: ^cEmutecaGroup;
begin
  pGroup := Sender.GetNodeData(Node);
  if pGroup^ = nil then
    Exit;
  case TextType of
    ttNormal: case Column of
        -1, 0: CellText := pGroup^.Title;
        1: CellText := pGroup^.ID;
      end;
  end;
end;

procedure TfmLEmuTKMediaManager.SetEmuteca(AValue: cEmuteca);
begin
  if FEmuteca = AValue then
    Exit;
  FEmuteca := AValue;

  if Assigned(Emuteca) then
    fmSystemCBX.SystemList := Emuteca.SystemManager.EnabledList
  else
    fmSystemCBX.SystemList := nil;

  LoadFrameData;
end;

procedure TfmLEmuTKMediaManager.SetExtFilter(AValue: TStrings);
begin
  if FExtFilter = AValue then
    Exit;
  FExtFilter := AValue;
end;

procedure TfmLEmuTKMediaManager.SetGUIConfig(AValue: cGUIConfig);
begin
  if FGUIConfig = AValue then
    Exit;
  FGUIConfig := AValue;

  if assigned(GUIConfig) then
  begin
    GUIConfigIni := GUIConfig.ConfigFile;
    SHA1Folder := SetAsAbsoluteFile(GUIConfig.GlobalCache, ProgramDirectory);
  end
  else
  begin
    GUIConfigIni := '';
    SHA1Folder := '';
  end;
end;

procedure TfmLEmuTKMediaManager.SetCurrSystem(AValue: cEmutecaSystem);
begin
  if FCurrSystem = AValue then
    Exit;
  FCurrSystem := AValue;

  LoadSysFolders;
end;

procedure TfmLEmuTKMediaManager.SetCurrentGroup(AValue: cEmutecaGroup);
begin
  if FCurrentGroup = AValue then
    Exit;
  FCurrentGroup := AValue;
end;

class function TfmLEmuTKMediaManager.SimpleForm(aEmuteca: cEmuteca;
  aGUIIconsIni: string; aGUIConfig: cGUIConfig): integer;
var
  aForm: TfrmCHXForm;
  aFrame: TfmLEmuTKMediaManager;
begin
  Result := mrNone;

  Application.CreateForm(TfrmCHXForm, aForm);
  try
    aForm.Name := 'frmLEmuTKMediaManager';
    aForm.Caption := Format(rsFmtWindowCaption,
      [Application.Title, 'Media Manager']);

    // Set aForm.GUIConfigIni before creation of aFrame because
    //  it is set with aFrame.GUIConfig := aGUIConfig; later.
    aForm.GUIConfigIni := aGUIConfig.ConfigFile;

    aFrame := TfmLEmuTKMediaManager.Create(aForm);
    aFrame.Align := alClient;

    aFrame.GUIConfig := aGUIConfig;
    aFrame.Emuteca := aEmuteca;

    aForm.GUIIconsIni := aGUIIconsIni;
    aFrame.Parent := aForm;

    Result := aForm.ShowModal;
  finally
    aForm.Free;
  end;
end;

constructor TfmLEmuTKMediaManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  vstAllGroups.NodeDataSize := SizeOf(TObject);
  vstGroupsWOFile.NodeDataSize := SizeOf(TObject);
  vstAllSoft.NodeDataSize := SizeOf(TObject);
  vstSoftWOFile.NodeDataSize := SizeOf(TObject);
  vstAllFiles.NodeDataSize := SizeOf(string);
  vstFilesWOGroup.NodeDataSize := SizeOf(string);
  vstFilesWOSoft.NodeDataSize := SizeOf(string);
  vstOtherFiles.NodeDataSize := SizeOf(string);
  vstFilesOtherFolder.NodeDataSize := SizeOf(string);

  pcSource.ActivePageIndex := 0;
  pcTarget.ActivePageIndex := 0;

  FfmSystemCBX := TfmEmutecaSystemCBX.Create(gbxSystem);
  fmSystemCBX.FirstItem := ETKSysCBXFISelect;
  fmSystemCBX.OnSelectSystem := @SelectSystem;
  fmSystemCBX.Align := alTop;
  fmSystemCBX.Parent := gbxSystem;

  // If frmCHXProgressBar is not created...
  if not Assigned(frmCHXProgressBar) then
    Application.CreateForm(TfrmCHXProgressBar, frmCHXProgressBar);

end;

destructor TfmLEmuTKMediaManager.Destroy;
begin
  inherited Destroy;
end;

end.
