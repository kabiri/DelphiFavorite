{
  Unit Name   : WP.Favorite.Creator.pas
  Author      : Hamid Reza Kabiri
  Created     : 04/09/2025
  Description : Register the plugin on the welcome page

  History:
    -

  Notes:
}

unit WP.Favorite.Creator;

interface

uses
  System.SysUtils, System.Classes, Vcl.Forms, Vcl.Controls, Vcl.Graphics, Vcl.Dialogs,
  ToolsAPI.WelcomePage, ToolsAPI;

type
  TWPPlugInCreator = class(TInterfacedObject, INTAWelcomePagePlugin, INTAWelcomePageContentPluginCreator)
  private
    FWPPluginView: TFrame;
    FIconIndex: Integer;
    { INTAWelcomePageContentPluginCreator }
    function GetView: TFrame;
    function GetIconIndex: Integer;
    procedure SetIconIndex(const Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    class procedure PlugInStartup;
    class procedure PlugInFinish;
    { INTAWelcomePagePlugin }
    function GetPluginID: string;
    function GetPluginName: string;
    function GetPluginVisible: boolean;
    { INTAWelcomePageContentPluginCreator }
    function CreateView: TFrame;
    procedure DestroyView;
    function GetIcon: TGraphicArray;
  end;

procedure Register;

implementation

uses WP.Favorite.View;

Const
  cPluginID = 'WP.CFavorite';
  cPluginName = 'Cevahir Favorite';

procedure Register;
begin
  TWPPlugInCreator.PlugInStartup;
end;

{ TWPDemoPlugInCreator }

function TWPPlugInCreator.GetPluginID: string;
begin
  Result := cPluginID;
end;

function TWPPlugInCreator.GetPluginName: string;
begin
  Result := cPluginName;
end;

function TWPPlugInCreator.GetPluginVisible: boolean;
begin
  Result := True;
end;

constructor TWPPlugInCreator.Create;
begin
  FIconIndex := -1;
end;

destructor TWPPlugInCreator.Destroy;
begin
  DestroyView;
  inherited;
end;

function TWPPlugInCreator.CreateView: TFrame;
var
  LPluginView: INTAWelcomePageCaptionFrame;
begin
  if not Assigned(FWPPluginView) then
    FWPPluginView := WelcomePagePluginService.CreateCaptionFrame(cPluginID, cPluginName, nil);

  if Supports(FWPPluginView, INTAWelcomePageCaptionFrame, LPluginView) then
  begin
    MainFrame := TfrmMain.Create(FWPPluginView);
    LPluginView.SetClientFrame(MainFrame);
  end;
  Result := FWPPluginView;
end;

procedure TWPPlugInCreator.DestroyView;
begin
  FreeAndNil(FWPPluginView);
end;

function TWPPlugInCreator.GetIcon: TGraphicArray;
begin
  Result := [];
end;

function TWPPlugInCreator.GetIconIndex: Integer;
begin
  Result := FIconIndex;
end;

procedure TWPPlugInCreator.SetIconIndex(const Value: Integer);
begin
  FIconIndex := Value;
end;

function TWPPlugInCreator.GetView: TFrame;
begin
  Result := FWPPluginView;
end;

class procedure TWPPlugInCreator.PlugInStartup;
begin
  WelcomePagePluginService.RegisterPluginCreator(TWPPlugInCreator.Create);
end;

class procedure TWPPlugInCreator.PlugInFinish;
begin
  if Assigned(WelcomePagePluginService) then
    WelcomePagePluginService.UnRegisterPlugin(cPluginID);
end;

initialization

finalization
  TWPPlugInCreator.PlugInFinish;

end.
