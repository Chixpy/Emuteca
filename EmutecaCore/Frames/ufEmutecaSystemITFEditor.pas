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

{ TfmEmutecaSystemITFEditor }

procedure TfmEmutecaSystemITFEditor.SetSystem(AValue: cEmutecaSystem);
begin
  if FSystem = AValue then Exit;
  FSystem := AValue;

  if Assigned(System) then
  begin
    fmImageFolders.FolderList := System.ImageFolders;
    fmImageFolders.CaptionList := System.ImageCaptions;
    fmTextFolders.FolderList := System.TextFolders;
    fmTextFolders.CaptionList := System.TextCaptions;
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

procedure TfmEmutecaSystemITFEditor.SetGUIIconsIni(AValue: string);
begin
  inherited SetGUIIconsIni(AValue);
end;

procedure TfmEmutecaSystemITFEditor.SetGUIConfigIni(AValue: string);
begin
  inherited SetGUIConfigIni(AValue);
end;

procedure TfmEmutecaSystemITFEditor.ClearFrameData;
begin

end;

procedure TfmEmutecaSystemITFEditor.LoadFrameData;
begin
  Enabled := assigned(System);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;
end;

procedure TfmEmutecaSystemITFEditor.SaveFrameData;
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
end;

destructor TfmEmutecaSystemITFEditor.Destroy;
begin
  inherited Destroy;
end;

end.

