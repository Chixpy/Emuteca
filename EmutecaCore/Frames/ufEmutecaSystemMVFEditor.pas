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

    procedure SetGUIIconsIni(AValue: string); override;
    procedure SetGUIConfigIni(AValue: string); override;

    procedure ClearFrameData; override;
    procedure LoadFrameData; override;

  public
    property System: cEmutecaSystem read FSystem write SetSystem;

    procedure SaveFrameData; override;

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
    fmVideoFolders.FolderList := System.VideoFolders;
    fmVideoFolders.CaptionList := System.VideoCaptions;
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

procedure TfmEmutecaSystemMVFEditor.SetGUIIconsIni(AValue: string);
begin
  inherited SetGUIIconsIni(AValue);
end;

procedure TfmEmutecaSystemMVFEditor.SetGUIConfigIni(AValue: string);
begin
  inherited SetGUIConfigIni(AValue);
end;

procedure TfmEmutecaSystemMVFEditor.ClearFrameData;
begin
end;

procedure TfmEmutecaSystemMVFEditor.LoadFrameData;
begin
  Enabled := assigned(System);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;
end;

procedure TfmEmutecaSystemMVFEditor.SaveFrameData;
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
end;

destructor TfmEmutecaSystemMVFEditor.Destroy;
begin
  inherited Destroy;
end;

end.
