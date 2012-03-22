unit fConfigManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, uConfig;

type

  { TfrmConfigManager }

  TfrmConfigManager = class(TForm)
    gbxImageExt: TGroupBox;
    mImageExt: TMemo;

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
  mImageExt.Clear;
  mImageExt.Lines.AddStrings(AValue.ImageExtensions);

end;

initialization
  {$I fConfigManager.lrs}

end.

