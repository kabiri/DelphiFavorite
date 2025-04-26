{
  Unit Name   : WP.Favorite.Setting.pas
  Author      : Hamid Reza Kabiri
  Created     : 04/09/2025
  Description :

  History:
    -

  Notes:
}

unit WP.Favorite.Setting;

interface

uses
  ToolsAPI,
  System.Classes,
  System.SysUtils,
  Vcl.Forms;

type
  TSingletonSettings = class
  private
    class var FInstance: TSingletonSettings;
    class function GetInstance: TSingletonSettings; static;
    constructor Create;
  public
    class procedure RegisterFormClassForTheming(const AFormClass: TCustomFormClass; const Component: TComponent); static;
    class property Instance: TSingletonSettings read GetInstance;
  end;

implementation

{ TSingletonSettings }

constructor TSingletonSettings.Create;
begin
  inherited;
end;

class function TSingletonSettings.GetInstance: TSingletonSettings;
begin
  if not Assigned(FInstance) then
    FInstance := TSingletonSettings.Create;
  Result := FInstance;
end;

class procedure TSingletonSettings.RegisterFormClassForTheming(
  const AFormClass: TCustomFormClass; const Component: TComponent);
var
 ITS: IOTAIDEThemingServices;
begin
  if Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) then
  begin
    if ITS.IDEThemingEnabled then
    begin
      ITS.RegisterFormClass(AFormClass);
      if Assigned(Component) then
        ITS.ApplyTheme(Component);
    end;
  end;
end;

end.
