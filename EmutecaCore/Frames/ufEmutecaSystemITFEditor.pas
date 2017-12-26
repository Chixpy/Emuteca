unit ufEmutecaSystemITFEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ActnList, StdCtrls,
  ufCHXPropEditor, ufCHXMultiFolderEditor,
  ucEmutecaSystem;

type

  { TfmEmutecaSystemITFEditor }

  TfmEmutecaSystemITFEditor = class(TfmCHXPropEditor)
    gbxImageFolders: TGroupBox;
    gbxTextFolders: TGroupBox;
    Splitter1: TSplitter;

  private
    FfmImageFolders: TfmCHXMultiFolderEditor;
    FSystem: cEmutecaSystem;
    FfmTextFolders: TfmCHXMultiFolderEditor;
    procedure SetSystem(AValue: cEmutecaSystem);

  protected
    property fmImageFolders: TfmCHXMultiFolderEditor read FfmImageFolders;
    property fmTextFolders: TfmCHXMultiFolderEditor read FfmTextFolders;

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

{ TfmEmutecaSystemITFEditor }

procedure TfmEmutecaSystemITFEditor.SetSystem(AValue: cEmutecaSystem);
begin
  if FSystem = AValue then
    Exit;
  FSystem := AValue;

  if Assigned(System) then
  begin
    fmImageFolders.FolderList := System.ImageFolders;
    fmImageFolders.CaptionList := System.ImageCaptions;
    fmImageFolders.InitialFolder :=System.BaseFolder;
    fmTextFolders.FolderList := System.TextFolders;
    fmTextFolders.CaptionList := System.TextCaptions;
    fmTextFolders.InitialFolder :=System.BaseFolder;
  end
  else
  begin
    fmImageFolders.FolderList := nil;
    fmImageFolders.CaptionList := nil;
    fmTextFolders.FolderList := nil;
    fmTextFolders.CaptionList := nil;
  end;

  LoadFrameData;
end;

procedure TfmEmutecaSystemITFEditor.DoClearFrameData;
begin

end;

procedure TfmEmutecaSystemITFEditor.DoLoadFrameData;
begin
  Enabled := assigned(System);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;
end;

procedure TfmEmutecaSystemITFEditor.DoSaveFrameData;
begin
  fmImageFolders.SaveFrameData;
  fmTextFolders.SaveFrameData;
end;

constructor TfmEmutecaSystemITFEditor.Create(TheOwner: TComponent);

  procedure CreateFrames;
  begin
    FfmImageFolders := TfmCHXMultiFolderEditor.Create(gbxImageFolders);
    fmImageFolders.SaveButtons := False;
    fmImageFolders.ButtonClose := False;
    fmImageFolders.Align := alClient;
    fmImageFolders.Parent := gbxImageFolders;

    FfmTextFolders := TfmCHXMultiFolderEditor.Create(gbxTextFolders);
    fmTextFolders.SaveButtons := False;
    fmTextFolders.ButtonClose := False;
    fmTextFolders.Align := alClient;
    fmTextFolders.Parent := gbxTextFolders;
  end;

begin
  inherited Create(TheOwner);

  CreateFrames;

  OnClearFrameData := @DoClearFrameData;
  OnLoadFrameData := @DoLoadFrameData;
  OnSaveFrameData := @DoSaveFrameData;
end;

destructor TfmEmutecaSystemITFEditor.Destroy;
begin
  inherited Destroy;
end;

end.
