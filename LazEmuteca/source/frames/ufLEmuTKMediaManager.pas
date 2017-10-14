unit ufLEmuTKMediaManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, VirtualTrees, Forms, Controls,
  Graphics, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, ActnList, LCLType, Buttons, EditBtn, Menus,
  LazFileUtils, LazUTF8, uCHXStrUtils, uCHXImageUtils,
  uCHXFileUtils, ufCHXForm,
  ufCHXFrame, ufCHXStrLstPreview, ufCHXImgListPreview, ufCHXTxtListPreview,
  ufCHXProgressBar, uEmutecaCommon, ucEmuteca, ucEmutecaSystem, ucEmutecaGroup,
  ucEmutecaSoftware, ufEmutecaSystemCBX, uGUIConfig, uLEmuTKCommon;

type

  { TfmLEmuTKMediaManager }

  TfmLEmuTKMediaManager = class(TfmCHXFrame)
    actAssignFile: TAction;
    actDeleteFile: TAction;
    actRenameGroupTitle: TAction;
    actSearchMediaInZip: TAction;
    actRenameGroupFile: TAction;
    ActionList: TActionList;
    bRename: TBitBtn;
    bSearchMediaInZip: TButton;
    chkCopyFile: TCheckBox;
    chkSimilarFiles: TCheckBox;
    eOtherFolder: TDirectoryEdit;
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
    migpRenameGroupFile: TMenuItem;
    migpRenameGroupTitle: TMenuItem;
    MenuItem2: TMenuItem;
    migpAssignFile: TMenuItem;
    pCenter: TPanel;
    pcSource: TPageControl;
    pcTarget: TPageControl;
    pImagePreview: TPanel;
    pLeft: TScrollBox;
    pRight: TPanel;
    pSimilar: TPanel;
    pTargetButtons: TPanel;
    pTextPreview: TPanel;
    pumVSTGroup: TPopupMenu;
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
    Splitter4: TSplitter;
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
    procedure actAssignFileExecute(Sender: TObject);
    procedure actDeleteFileExecute(Sender: TObject);
    procedure actRenameGroupFileExecute(Sender: TObject);
    procedure actRenameGroupTitleExecute(Sender: TObject);
    procedure actSearchMediaInZipExecute(Sender: TObject);
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
    FCurrGroup: cEmutecaGroup;
    FCurrPreview: TfmCHXStrLstPreview;
    FCurrSystem: cEmutecaSystem;
    FEmuteca: cEmuteca;
    FExtFilter: TStrings;
    FfmImagePreview: TfmCHXImgListPreview;
    FfmSystemCBX: TfmEmutecaSystemCBX;
    FfmTextPreview: TfmCHXTxtListPreview;
    FGUIConfig: cGUIConfig;
    FMediaFiles: TStringList;
    FSHA1Folder: string;
    FSourceFile: string;
    FSourceFolder: string;
    FTargetFile: string;
    FTargetFolder: string;
    procedure SetCurrGroup(AValue: cEmutecaGroup);
    procedure SetCurrPreview(AValue: TfmCHXStrLstPreview);
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
    // Frames
    // ------
    property fmSystemCBX: TfmEmutecaSystemCBX read FfmSystemCBX;
    property fmImagePreview: TfmCHXImgListPreview read FfmImagePreview;
    property fmTextPreview: TfmCHXTxtListPreview read FfmTextPreview;

    // Properties
    // ----------
    property CurrSystem: cEmutecaSystem read FCurrSystem write SetCurrSystem;
    property CurrGroup: cEmutecaGroup read FCurrGroup write SetCurrGroup;

    property ExtFilter: TStrings read FExtFilter write SetExtFilter;
    {< Extensions of the current selected Media. }
    property CurrPreview: TfmCHXStrLstPreview
      read FCurrPreview write SetCurrPreview;
    property MediaFiles: TStringList read FMediaFiles;
    {< Mediafiles assigned to the current game or group. }

    property SourceFile: string read FSourceFile write SetSourceFile;
    {< Name of the source file. }
    property SourceFolder: string read FSourceFolder write SetSourceFolder;
    {< Folder of the source file. }
    property TargetFile: string read FTargetFile write SetTargetFile;
    {< Name of the target file. }
    property TargetFolder: string read FTargetFolder write SetTargetFolder;
    {< Folder of the target file. }

    procedure UpdateStatusBars;
    procedure UpdateVST(aFolder: string);
    {< Update the Virtual String Trees.
      @param(aFolder Folder where search the files and media.)
    }

    function SelectSystem(aSystem: cEmutecaSystem): boolean;
    {< Selects a system.
    }
    procedure LoadSysFolders;
    {< Load CurrSystem folders in lbx. }

    // File lists
    // ----------

    function GetCurrentFilesVST: TCustomVirtualStringTree;
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
    procedure RemoveFileVSTFiles(aFile: string);
    {< Remove a file from lists (not physically).
      If TargetFolder <> SourceFolder then removed from vstFilesOtherFolder.
      Used for hacky updates.
      @param(aFile Name of the file.)
    }


    // Group / Soft list
    procedure RemoveGroupSoftWOFile(aFile: string);
    {< Remove groups from vstGroupsWOFile and  vstSoftWOFile lists that have
         aFile.
      Used for hacky updates.
      @param(aFile Name of file that maybe is used by groups or soft.)
    }


    // Media
    procedure ChangeGroupMedia(aGroup: cEmutecaGroup);
    {< Change the media preview to the group media.
      @param(aGroup The game group with it's media will be previewed.)
    }
    procedure ChangeSoftMedia(aSoft: cEmutecaSoftware);
    {< Change the media preview to the soft media.
      @param(aGame The game with it's media will be previewed.)
    }
    procedure ChangeFileMedia(aFolder, aFileName: string);
    {< Change the media preview to the file.
      @param(aFolder Folder were the file is.)
      @param(aName Name of the file.)
    }





 //property MediaFiles: TStringList read FMediaFiles write SetMediaFiles;
 ////< Mediafiles assigned to the current game or group.
 //property CurrentMediaIndex: integer
 //  read FCurrentMediaIndex write SetCurrentMediaIndex;
 ////< Index of the current media file.
 //
 //// TODO 3: Maybe this 4 methods can be reduced to 2 without ofuscate them...
 //
 //
 //function AddFilesOtherFolder(aFolder: string;
 //  Info: TSearchRec): boolean; overload;
 //{< Add files or folders to vstFilesOtherFolder.
 //  @param(aFolder Folder where the file is in.)
 //  @param(Info TSearchRec with folder or file data.)
 //  @return(Always @true; needed for IterateFolder.)
 //}
 //
 //
 //procedure UpdateFileOtherFolder(const afolder: string);
 //
 //procedure DeleteAllFiles;
 //{< Delete all VISIBLE (i.e. no hidden) files from the current list
 //     FROM DISC PHYSICALLY. }
 //procedure MoveFile;
 //{< Move current selected (source) file to another folder. }
 //procedure MoveAllFiles;
 //{< Move all VISIBLE (i.e. no hidden) files from the current list to
 //    another folder. }




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

  // UpdateStatusBars; Updated by SetTargetFile
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
  sbSource.Panels[1].Text := SourceFolder + SourceFile;
  sbTarget.Panels[1].Text := TargetFolder + TargetFile;
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
  FileComp: integer;
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
  // ----------------------------------
  frmCHXProgressBar.UpdTextAndBar('Searching...',
    'Files without group and groups without file', '', 2, 4);

  // Sorting vstAllFiles and vstvstAllGroups to iterate them;
  vstAllFiles.SortTree(0, VirtualTrees.sdAscending, True); // By filename
  vstAllGroups.SortTree(1, VirtualTrees.sdAscending, True); // By filename

  vstFilesWOGroup.BeginUpdate;
  vstGroupsWOFile.BeginUpdate;
  GroupNode := vstAllGroups.GetFirstChild(nil);
  FileNode := vstAllFiles.GetFirstChild(nil);
  while (assigned(GroupNode)) or (assigned(FileNode)) do
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


  // vstFilesWOGroup -> vstFilesWOSoft and
  //   vstAllSoft -> vstSoftWOFile.
  // -------------------------------------
  frmCHXProgressBar.UpdTextAndBar('Searching...',
    'Files without soft and soft without file', '', 3, 4);

  // Sorting vstFilesWOGroup and vstvstAllGroups to iterate them;
  vstFilesWOGroup.SortTree(0, VirtualTrees.sdAscending, True); // By filename
  vstAllSoft.SortTree(2, VirtualTrees.sdAscending, True); // By filename

  vstFilesWOSoft.BeginUpdate;
  vstSoftWOFile.BeginUpdate;
  SoftNode := vstAllSoft.GetFirstChild(nil);
  FileNode := vstFilesWOGroup.GetFirstChild(nil);
  while (assigned(SoftNode)) or (assigned(FileNode)) do
  begin
    if not assigned(SoftNode) then
    begin
      aSoft := nil;
      pFilename := vstFilesWOGroup.GetNodeData(FileNode);
      aFileName := string(pFilename^);
      FileComp := -1;
    end
    else if not assigned(FileNode) then
    begin
      pSoft := vstAllSoft.GetNodeData(SoftNode);
      aSoft := pSoft^;
      aFileName := '';
      FileComp := 1;
    end
    else
    begin
      pFilename := vstFilesWOGroup.GetNodeData(FileNode);
      aFileName := string(pFilename^);
      pSoft := vstAllSoft.GetNodeData(SoftNode);
      aSoft := pSoft^;
      FileComp := CompareFilenames(ExtractFileNameOnly(aFileName),
        ExtractFileNameOnly(aSoft.FileName));
    end;


    if FileComp = 0 then
    begin  // Match!!
      FileNode := vstFilesWOGroup.GetNextSibling(FileNode);
      SoftNode := vstAllSoft.GetNextSibling(SoftNode);
    end
    else if FileComp < 0 then
    begin // Not match, File is behind Soft
      pFilename := vstFilesWOSoft.GetNodeData(
        vstFilesWOSoft.AddChild(nil));
      pFilename^ := aFileName;
      FileNode := vstFilesWOGroup.GetNextSibling(FileNode);
    end
    else  // Not match, Soft is behind Group File
    begin
      pSoft := vstSoftWOFile.GetNodeData(vstSoftWOFile.AddChild(nil));
      pSoft^ := aSoft;
      SoftNode := vstAllSoft.GetNextSibling(SoftNode);
    end;
  end;
  vstFilesWOSoft.EndUpdate;
  vstSoftWOFile.EndUpdate;

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
  aVST := GetCurrentFilesVST;
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
    if SupportedExtSL(aName, ExtFilter) then
      pFile := vstAllFiles.GetNodeData(vstAllFiles.AddChild(nil))
    else
      pFile := vstOtherFiles.GetNodeData(vstOtherFiles.AddChild(nil));
    // See: TfrmMediaManager.vstAllFilesFreeNode;
    pFile^ := aName;
  end;
end;

procedure TfmLEmuTKMediaManager.RemoveFileVSTFiles(aFile: string);

  procedure RemoveFileFromVST(aVST: TBaseVirtualTree; aFile: string);
  var
    pFileName: ^string;
    Nodo: PVirtualNode;
  begin
    aVST.BeginUpdate;

    // From LastChild
    Nodo := aVST.GetLastChild(nil);
    while (Nodo <> nil) do
    begin
      pFileName := aVST.GetNodeData(Nodo);
      if pFileName^ = aFile then
      begin
        aVST.DeleteNode(Nodo);
        // ...VST is too dynamic...
        Nodo := aVST.GetLastChild(nil);
      end
      else
        Nodo := aVST.GetPreviousSibling(Nodo);
    end;
    aVST.EndUpdate;
  end;

begin
  if CompareFilenames(SourceFolder, TargetFolder) = 0 then
  begin
    RemoveFileFromVST(vstAllFiles, aFile);
    RemoveFileFromVST(vstOtherFiles, aFile);
    RemoveFileFromVST(vstFilesWOGroup, aFile);
    RemoveFileFromVST(vstFilesWOSoft, aFile);
  end;

  if CompareFilenames(SourceFolder,
    SetAsFolder(eOtherFolder.Directory)) = 0 then
    RemoveFileFromVST(vstFilesOtherFolder, aFile);
end;

procedure TfmLEmuTKMediaManager.RemoveGroupSoftWOFile(aFile: string);
var
  Nodo: PVirtualNode;
  PGroup: ^cEmutecaGroup;
  pSoft: ^cEmutecaSoftware;
begin
  aFile := ExtractFileNameOnly(aFile);

  // vstGroupsWOFile
  vstGroupsWOFile.BeginUpdate;
  Nodo := vstGroupsWOFile.GetFirstChild(nil);
  while (Nodo <> nil) do
  begin
    PGroup := vstGroupsWOFile.GetNodeData(Nodo);
    if CompareFilenames(PGroup^.ID, aFile) = 0 then
    begin
      vstGroupsWOFile.DeleteNode(Nodo);
      // See RemoveFileFromVSTWO comment
      Nodo := vstGroupsWOFile.GetFirstChild(nil);
    end
    else
      Nodo := vstGroupsWOFile.GetNextSibling(Nodo);
  end;
  vstGroupsWOFile.EndUpdate;

  // vstSoftWOFile
  vstSoftWOFile.BeginUpdate;
  Nodo := vstSoftWOFile.GetFirstChild(nil);
  while (Nodo <> nil) do
  begin
    pSoft := vstSoftWOFile.GetNodeData(Nodo);
    if CompareFilenames(ExtractFileNameOnly(pSoft^.FileName), aFile) = 0 then
    begin
      vstSoftWOFile.DeleteNode(Nodo);
      // See RemoveFileFromVSTWO comment
      Nodo := vstSoftWOFile.GetFirstChild(nil);
    end
    else
      Nodo := vstSoftWOFile.GetNextSibling(Nodo);
  end;
  vstSoftWOFile.EndUpdate;

end;

procedure TfmLEmuTKMediaManager.ChangeGroupMedia(aGroup: cEmutecaGroup);
begin
  CurrPreview.StrList := nil;
  MediaFiles.Clear;
  aGroup.SearchAllRelatedFiles(MediaFiles, TargetFolder, ExtFilter, True);
  CurrPreview.StrList := MediaFiles;
end;

procedure TfmLEmuTKMediaManager.ChangeSoftMedia(aSoft: cEmutecaSoftware);
begin
  CurrPreview.StrList := nil;
  MediaFiles.Clear;
  aSoft.SearchAllRelatedFiles(MediaFiles, TargetFolder, ExtFilter, True);
  CurrPreview.StrList := MediaFiles;
end;

procedure TfmLEmuTKMediaManager.ChangeFileMedia(aFolder, aFileName: string);
begin
  CurrPreview.StrList := nil;
  MediaFiles.Clear;
  if SupportedExtSL(aFileName, ExtFilter) then
    MediaFiles.Add(aFolder + aFileName);
  CurrPreview.StrList := MediaFiles;
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

function TfmLEmuTKMediaManager.GetCurrentFilesVST: TCustomVirtualStringTree;
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
  if Assigned(CurrPreview) then
    CurrPreview.StrList := nil;
  CurrPreview := nil;

  if aLBX = lbxImages then
  begin
    case aLBX.ItemIndex of
      0: UpdateVST(CurrSystem.IconFolder);
      else
        UpdateVST(CurrSystem.ImageFolders[aLBX.ItemIndex - 1]);
    end;
    if Assigned(GUIConfig) then
      ExtFilter := GUIConfig.ImageExtensions;
    CurrPreview := fmImagePreview;
  end
  else if aLBX = lbxTexts then
  begin
    UpdateVST(CurrSystem.TextFolders[aLBX.ItemIndex]);
    if Assigned(GUIConfig) then
      ExtFilter := GUIConfig.TextExtensions;
    CurrPreview := fmTextPreview;
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

  ChangeFileMedia(SourceFolder, SourceFile);
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
      VK_RETURN: actAssignFile.Execute;
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
  CurrGroup := PData^;

  TargetFile := CurrGroup.MediaFileName + krsVirtualExt;
  ChangeGroupMedia(CurrGroup);
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
    1: Result := CompareFilenames(pGroup1^.MediaFileName, pGroup2^.MediaFileName);
  end;
end;

procedure TfmLEmuTKMediaManager.vstGroupKeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin
  if Shift = [] then
  begin
    case Key of
      VK_RETURN: actAssignFile.Execute;
    end;
  end;
end;

procedure TfmLEmuTKMediaManager.chkSimilarFilesChange(Sender: TObject);
begin
  FilterFiles;
end;

procedure TfmLEmuTKMediaManager.actAssignFileExecute(Sender: TObject);
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
    // HACK: Where is CopyFileUTF8 and CopyDirTreeUTF8?
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
    RemoveFileVSTFiles(SourceFile);
  end;


  // Quick update of VST lists
  // -------------------------

  // Removing Target file from vstGroupWOFile or vstSoftWOFile.
  RemoveGroupSoftWOFile(TargetFile);

  // Adding TargetFile.
  AddFile(TargetFolder, TargetFile);

  SourceFile := '';

  // Clear TargetFile only if pagGroupsWOFile is active because
  //  the group is deleted from its list.
  if GetCurrentFilesVST = vstGroupsWOFile then
  begin
    TargetFile := '';
  end;

  FilterFiles;
end;

procedure TfmLEmuTKMediaManager.actDeleteFileExecute(Sender: TObject);
begin
  if (SourceFile = '') or (SourceFolder = '') then
    Exit;

  if MessageDlg(Format('Do you want delete? %0:s',
    [SourceFolder + SourceFile]), mtConfirmation, [mbYes, mbNo],
    -1) = mrNo then
    Exit;

  if not DeleteFileUTF8(SourceFolder + SourceFile) then
  begin
    ShowMessageFmt('Error deleting: %0:s', [SourceFolder + SourceFile]);
    Exit;
  end;

  RemoveFileVSTFiles(SourceFile);
  SourceFile := '';
end;

procedure TfmLEmuTKMediaManager.actRenameGroupFileExecute(Sender: TObject);
var
  NewName: string;
begin
  if not Assigned(CurrGroup) then Exit;
  NewName := CleanFileName(CurrGroup.SortTitle);

  if not InputQuery(self.Caption, 'Rename group media filename:', NewName) then Exit;

  CurrGroup.MediaFileName := NewName;

    TargetFile := CurrGroup.MediaFileName + krsVirtualExt;
  ChangeGroupMedia(CurrGroup);
  FilterFiles;
end;

procedure TfmLEmuTKMediaManager.actRenameGroupTitleExecute(Sender: TObject);
var
  NewName: String;
begin
  if not Assigned(CurrGroup) then Exit;

   NewName := CurrGroup.Title;

  if not InputQuery(self.Caption, 'Rename group title:', NewName) then Exit;

  CurrGroup.Title := NewName;

  if MessageDlg(self.Caption, 'Rename group media filename?', mtConfirmation, mbYesNo,
   '') = mrNo then Exit;

  CurrGroup.MediaFileName := CleanFileName(NewName);

  TargetFile := CurrGroup.MediaFileName + krsVirtualExt;
  ChangeGroupMedia(CurrGroup);
  FilterFiles;
end;

procedure TfmLEmuTKMediaManager.actSearchMediaInZipExecute(Sender: TObject);
var
  Nodo: PVirtualNode;
  pGroup: ^cEmutecaGroup;
  pSoft: ^cEmutecaSoftware;
  aFile: string;
begin
  frmCHXProgressBar.UpdTextAndBar('Searching...',
    'Files without group and groups without file', '', 1, 4);

  // vstGroupsWOFile
  vstGroupsWOFile.BeginUpdate;
  Nodo := vstGroupsWOFile.GetFirstChild(nil);
  while (Nodo <> nil) do
  begin
    pGroup := vstGroupsWOFile.GetNodeData(Nodo);

    aFile := pGroup^.SearchFirstRelatedFile(TargetFolder, ExtFilter, False);

    if aFile = '' then
    begin
      vstGroupsWOFile.DeleteNode(Nodo);
      // See RemoveFileFromVSTWO comment...
      //   ... but this time it's tooo slow
      Nodo := vstGroupsWOFile.GetFirstChild(nil);
    end
    else
      Nodo := vstGroupsWOFile.GetNextSibling(Nodo);
  end;
  vstGroupsWOFile.EndUpdate;

  frmCHXProgressBar.UpdTextAndBar('Searching...',
    'Files without soft and soft without file', '', 2, 4);
  // vstSoftWOFile
  vstSoftWOFile.BeginUpdate;
  Nodo := vstSoftWOFile.GetFirstChild(nil);
  while (Nodo <> nil) do
  begin
    pSoft := vstSoftWOFile.GetNodeData(Nodo);

    aFile := pSoft^.SearchFirstRelatedFile(TargetFolder, ExtFilter, False);

    if aFile = '' then
    begin
      vstSoftWOFile.DeleteNode(Nodo);
      // See RemoveFileFromVSTWO comment...
      //   ... but this time it's tooo slow
      Nodo := vstSoftWOFile.GetFirstChild(nil);
    end
    else
      Nodo := vstSoftWOFile.GetNextSibling(Nodo);
  end;
  vstSoftWOFile.EndUpdate;

  frmCHXProgressBar.UpdateProgressBar(0, 0);
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
  CurrGroup := cEmutecaGroup(pSoft^.CachedGroup);

  ChangeSoftMedia(pSoft^);

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
        1: CellText := pGroup^.MediaFileName;
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
  fmSystemCBX.SelectedSystem := nil;

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

procedure TfmLEmuTKMediaManager.SetCurrGroup(AValue: cEmutecaGroup);
begin
  if FCurrGroup = AValue then
    Exit;
  FCurrGroup := AValue;
end;

procedure TfmLEmuTKMediaManager.SetCurrPreview(AValue: TfmCHXStrLstPreview);
begin
  if FCurrPreview = AValue then
    Exit;
  FCurrPreview := AValue;
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
    aForm.Caption := Format(krsFmtWindowCaption,
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

  procedure CreateFrames;
  begin
    FfmSystemCBX := TfmEmutecaSystemCBX.Create(gbxSystem);
    fmSystemCBX.FirstItem := ETKSysCBXFISelect;
    fmSystemCBX.OnSelectSystem := @SelectSystem;
    fmSystemCBX.Align := alTop;
    fmSystemCBX.Parent := gbxSystem;

    FfmImagePreview := TfmCHXImgListPreview.Create(pImagePreview);
    fmImagePreview.Align := alClient;
    fmImagePreview.Parent := pImagePreview;

    FfmTextPreview := TfmCHXTxtListPreview.Create(pTextPreview);
    fmTextPreview.Align := alClient;
    fmTextPreview.Parent := pTextPreview;
  end;

begin
  inherited Create(TheOwner);

  vstAllGroups.NodeDataSize := SizeOf(TObject);
  vstAllSoft.NodeDataSize := SizeOf(TObject);
  vstGroupsWOFile.NodeDataSize := SizeOf(TObject);
  vstSoftWOFile.NodeDataSize := SizeOf(TObject);
  vstAllFiles.NodeDataSize := SizeOf(string);
  vstOtherFiles.NodeDataSize := SizeOf(string);
  vstFilesWOGroup.NodeDataSize := SizeOf(string);
  vstFilesWOSoft.NodeDataSize := SizeOf(string);
  vstFilesOtherFolder.NodeDataSize := SizeOf(string);

  pcSource.ActivePageIndex := 0;
  pcTarget.ActivePageIndex := 0;

  FMediaFiles := TStringList.Create;

  // If frmCHXProgressBar is not created...
  if not Assigned(frmCHXProgressBar) then
    Application.CreateForm(TfrmCHXProgressBar, frmCHXProgressBar);

  CreateFrames;
end;

destructor TfmLEmuTKMediaManager.Destroy;
begin
  MediaFiles.Free;

  inherited Destroy;
end;

end.
