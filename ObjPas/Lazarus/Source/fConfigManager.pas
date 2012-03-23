unit fConfigManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, EditBtn, uConfig;

type

  { TfrmConfigManager }

  TfrmConfigManager = class(TForm)
    e7zPath: TFileNameEdit;
    emPlayerPath: TFileNameEdit;
    gbxImageExt: TGroupBox;
    gbxCompExt: TGroupBox;
    gbxTextExt: TGroupBox;
    l7zPath: TLabel;
    lmPlayerPath: TLabel;
    mCompExt: TMemo;
    mTextExt: TMemo;
    mImageExt: TMemo;
    p7zPath: TPanel;
    pmPlayerPath: TPanel;
    pBottom: TPanel;
    pcConfig: TPageControl;
    pagExtensions: TTabSheet;
    pagPaths: TTabSheet;

  private
    { private declarations }
    FConfig: cConfig;
    procedure SetConfig(AValue: cConfig);

  public
    { public declarations }
    property Config: cConfig read FConfig write SetConfig;

  end;

var
  frmConfigManager: TfrmConfigManager;

implementation

{ TfrmConfigManager }

procedure TfrmConfigManager.SetConfig(AValue: cConfig);
begin
  FConfig := AValue;

  if AValue = nil then Exit;

  // Paths
  e7zPath.Text:= AValue.z7Subfolder + AValue.z7CMExecutable;
  emPlayerPath.Text:=AValue.mPlayerSubfolder + AValue.mPlayerExecutable;

  // Extensions
  mImageExt.Clear;
  mImageExt.Lines.AddStrings(AValue.ImageExtensions);
  mTextExt.Clear;
  mTextExt.Lines.AddStrings(AValue.TextExtensions);
  mCompExt.Clear;
  mCompExt.Lines.AddStrings(AValue.CompressedExtensions);

end;

initialization
  {$I fConfigManager.lrs}

end.

