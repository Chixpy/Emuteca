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


  public
    property System: cEmutecaSystem read FSystem write SetSystem;

    procedure LoadFrameData; override;
    procedure SaveFrameData;  override;

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
    fmMusicFolders.InitialFolder := System.BaseFolder;
    fmVideoFolders.FolderList := System.VideoFolders;
    fmVideoFolders.CaptionList := System.VideoCaptions;
    fmVideoFolders.InitialFolder := System.BaseFolder;
  end
  else
  begin
    fmMusicFolders.FolderList := nil;
    fmMusicFolders.CaptionList := nil;
    fmVideoFolders.FolderList := nil;
    fmVideoFolders.CaptionList := nil;
  end;
  LoadFrameData;
end;

procedure TfmEmutecaSystemMVFEditor.LoadFrameData;
begin
  inherited LoadFrameData;

  Enabled := assigned(System);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;
end;

procedure TfmEmutecaSystemMVFEditor.SaveFrameData;
begin
  inherited SaveFrameData;

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
end;

destructor TfmEmutecaSystemMVFEditor.Destroy;
begin
  inherited Destroy;
end;

initialization
  RegisterClass(TfmEmutecaSystemMVFEditor);

finalization
  UnRegisterClass(TfmEmutecaSystemMVFEditor);

end.
