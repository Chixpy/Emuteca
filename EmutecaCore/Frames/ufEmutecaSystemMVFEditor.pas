unit ufEmutecaSystemMVFEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ActnList, StdCtrls,
  ufCHXPropEditor, ufCHXMultiFolderEditor,
  ucEmutecaSystem;

type

  { TfmEmutecaSystemMVFEditor }

  TfmEmutecaSystemMVFEditor = class(TfmCHXPropEditor)
    gbxMusicFolders: TGroupBox;
    gbxVideoFolders: TGroupBox;
    Splitter1: TSplitter;

  private
    FfmMusicFolders: TfmCHXMultiFolderEditor;
    FSystem: cEmutecaSystem;
    FfmVideoFolders: TfmCHXMultiFolderEditor;
    procedure SetSystem(AValue: cEmutecaSystem);

  protected
    property fmMusicFolders: TfmCHXMultiFolderEditor read FfmMusicFolders;
    property fmVideoFolders: TfmCHXMultiFolderEditor read FfmVideoFolders;

    procedure DoClearFrameData;
    procedure DoLoadFrameData;
    procedure DoSaveFrameData;

  public
    property System: cEmutecaSystem read FSystem write SetSystem;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmEmutecaSystemMVFEditor }

procedure TfmEmutecaSystemMVFEditor.SetSystem(AValue: cEmutecaSystem);
begin
  if FSystem = AValue then
    Exit;
  FSystem := AValue;


  if Assigned(System) then
  begin
    fmMusicFolders.FolderList := System.MusicFolders;
    fmMusicFolders.CaptionList := System.MusicCaptions;
    fmMusicFolders.BaseFolder:=System.BaseFolder;
    fmVideoFolders.FolderList := System.VideoFolders;
    fmVideoFolders.CaptionList := System.VideoCaptions;
    fmVideoFolders.BaseFolder:=System.BaseFolder;
  end
  else
  begin
    fmMusicFolders.FolderList := nil;
    fmMusicFolders.CaptionList := nil;
    fmMusicFolders.BaseFolder := '';
    fmVideoFolders.FolderList := nil;
    fmVideoFolders.CaptionList := nil;
    fmVideoFolders.BaseFolder := '';
  end;
  LoadFrameData;
end;

procedure TfmEmutecaSystemMVFEditor.DoClearFrameData;
begin
end;

procedure TfmEmutecaSystemMVFEditor.DoLoadFrameData;
begin
  Enabled := assigned(System);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;
end;

procedure TfmEmutecaSystemMVFEditor.DoSaveFrameData;
begin
  fmMusicFolders.SaveFrameData;
  fmVideoFolders.SaveFrameData;
end;

constructor TfmEmutecaSystemMVFEditor.Create(TheOwner: TComponent);

  procedure CreateFrames;
  begin
    FfmMusicFolders := TfmCHXMultiFolderEditor.Create(gbxMusicFolders);
    fmMusicFolders.SaveButtons := False;
    fmMusicFolders.ButtonClose := False;
    fmMusicFolders.Align := alClient;
    fmMusicFolders.Parent := gbxMusicFolders;

    FfmVideoFolders := TfmCHXMultiFolderEditor.Create(gbxVideoFolders);
    fmVideoFolders.SaveButtons := False;
    fmVideoFolders.ButtonClose := False;
    fmVideoFolders.Align := alClient;
    fmVideoFolders.Parent := gbxVideoFolders;
  end;

begin
  inherited Create(TheOwner);

  CreateFrames;

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;
  OnSaveFrameData := @DoSaveFrameData;
end;

destructor TfmEmutecaSystemMVFEditor.Destroy;
begin
  inherited Destroy;
end;

end.
