#
# Provides a tray applet that shows whether TLP has a sub-100% battery
# charge cap configured, and lets the user enable or disable that cap.
#
# The configured cap is read from TLP's STOP_CHARGE_THRESH_BAT0 setting
# in /run/tlp/run.conf, the actual active charge cap is read from sysfs,
# and the configured cap is activated and deactivated with `tlp fullcharge`
# and `tlp setcharge` respectively.
#
# Writen by AI with human oversight
#
# Icons are Apache-2.0 Pictogrammers Material Design Icons from @mdi/svg.
#

{ config, lib, pkgs, ... }: let

material-design-icons-svg = pkgs.fetchFromGitHub {
  owner = "Templarian";
  repo = "MaterialDesign-SVG";
  tag = "v7.4.47";
  hash = "sha256-NoSSRT1ID38MT70IZ+7h/gMVCNsjNs3A2RX6ePGwuQ0=";
};

battery-charge-threshold-tray-applet = pkgs.stdenv.mkDerivation {
  pname = "battery-charge-threshold-tray-applet";
  version = "1";

  dontUnpack = true;

  nativeBuildInputs = [ pkgs.pkg-config ];
  buildInputs = [
    pkgs.gtk3
    pkgs.libayatana-appindicator
  ];

  buildPhase = ''
    $CC -std=c11 -Wall -Wextra \
      -DFULL_ICON=\"$out/share/battery-charge-threshold-tray-applet/battery-charge-threshold-full.svg\" \
      -DCAPPED_ICON=\"$out/share/battery-charge-threshold-tray-applet/battery-charge-threshold-capped.svg\" \
      ${pkgs.writeText "battery-charge-threshold-tray-applet.c" ''
        #include <stdlib.h>
        #include <string.h>
        #include <sys/wait.h>

        #include <gtk/gtk.h>
        #include <libayatana-appindicator/app-indicator.h>

        static AppIndicator *indicator;
        static GtkWidget *cap_item;
        static GtkWidget *status_item;
        static gboolean is_capped;
        static gboolean is_refreshing;
        static gboolean is_changing;
        static long charge_cap;

        static gboolean read_charge_cap()
        {
            gchar *config_text = NULL;
            if (!g_file_get_contents("/run/tlp/run.conf", &config_text, NULL, NULL)) {
                return FALSE;
            }

            gboolean found = FALSE;
            gchar **lines = g_strsplit(config_text, "\n", -1);
            for (gchar **line = lines; *line != NULL; line++) {
                if (!g_str_has_prefix(*line, "STOP_CHARGE_THRESH_BAT0=")) {
                    continue;
                }

                gchar *value = g_strdup(*line + strlen("STOP_CHARGE_THRESH_BAT0="));
                g_strstrip(value);
                g_strdelimit(value, "\"'", ' ');
                g_strstrip(value);

                gchar *end = NULL;
                long parsed = strtol(value, &end, 10);
                found = end != value && *end == '\0' && parsed > 0 && parsed < 100;
                if (found) {
                    charge_cap = parsed;
                }

                g_free(value);
                break;
            }

            g_strfreev(lines);
            g_free(config_text);
            return found;
        }

        static gboolean refresh(gpointer unused)
        {
            (void) unused;

            gboolean cap_read = read_charge_cap();
            gchar *threshold_text = NULL;
            gboolean threshold_read =
                g_file_get_contents(
                    "/sys/class/power_supply/BAT0/charge_control_end_threshold",
                    &threshold_text,
                    NULL,
                    NULL);
            long threshold = threshold_read ? strtol(threshold_text, NULL, 10) : 100;
            g_free(threshold_text);

            is_capped = cap_read && threshold_read && threshold == charge_cap;
            gboolean is_full_charge_else_capped = threshold >= 100;
            const gchar *charge_state =
                is_full_charge_else_capped
                    ? "Full battery charge"
                    : "Capped battery charge";

            app_indicator_set_icon_full(
                indicator,
                is_full_charge_else_capped
                    ? FULL_ICON
                    : CAPPED_ICON,
                charge_state);
            gchar *label =
                is_full_charge_else_capped
                    ? g_strdup("FULL")
                    : cap_read
                        ? g_strdup_printf("%ld%%", charge_cap)
                        : g_strdup("?");
            app_indicator_set_label(indicator, label, "FULL");
            g_free(label);
            app_indicator_set_title(indicator, charge_state);

            gchar *cap_label = cap_read
                ? g_strdup_printf("Cap battery charge to %ld%%", charge_cap)
                : g_strdup("Cap battery charge");
            gtk_menu_item_set_label(GTK_MENU_ITEM(cap_item), cap_label);
            g_free(cap_label);

            is_refreshing = TRUE;
            gtk_check_menu_item_set_active(
                GTK_CHECK_MENU_ITEM(cap_item), is_capped);
            is_refreshing = FALSE;
            gtk_widget_set_sensitive(cap_item, cap_read && threshold_read && !is_changing);

            if (!cap_read) {
                gtk_menu_item_set_label(
                    GTK_MENU_ITEM(status_item), "Failed to read TLP capped threshold");
            } else if (!threshold_read) {
                gtk_menu_item_set_label(
                    GTK_MENU_ITEM(status_item), "Failed to read active battery threshold");
            } else if (!is_changing) {
                gtk_menu_item_set_label(GTK_MENU_ITEM(status_item), " ");
            }
            return G_SOURCE_CONTINUE;
        }

        static void charge_cap_changed(GPid pid, gint status, gpointer unused)
        {
            (void) unused;

            g_spawn_close_pid(pid);
            is_changing = FALSE;
            refresh(NULL);

            if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) {
                gtk_menu_item_set_label(
                    GTK_MENU_ITEM(status_item), "Failed to change battery charge cap");
            }
        }

        static void set_charge_cap(GtkCheckMenuItem *item, gpointer unused)
        {
            (void) unused;

            gboolean requested_capped = gtk_check_menu_item_get_active(item);
            if (is_refreshing || is_changing || requested_capped == is_capped) {
                return;
            }

            const gchar *command = requested_capped ? "setcharge" : "fullcharge";
            gchar *argv[] = {
                "${config.security.wrapperDir}/sudo",
                "${pkgs.tlp}/bin/tlp",
                (gchar *) command,
                NULL,
            };
            GError *error = NULL;
            GPid pid;

            gtk_menu_item_set_label(GTK_MENU_ITEM(status_item), "Changing battery charge cap...");
            is_changing = TRUE;
            gtk_widget_set_sensitive(cap_item, FALSE);

            gboolean spawned = g_spawn_async(
                NULL,
                argv,
                NULL,
                G_SPAWN_DO_NOT_REAP_CHILD,
                NULL,
                NULL,
                &pid,
                &error);
            if (spawned) {
                g_child_watch_add(pid, charge_cap_changed, NULL);
            } else {
                g_warning("Failed to run tlp: %s", error->message);
                g_clear_error(&error);
                is_changing = FALSE;
                refresh(NULL);
                gtk_menu_item_set_label(
                    GTK_MENU_ITEM(status_item), "Failed to change battery charge cap");
            }
        }

        int main(int argc, char **argv)
        {
            gtk_init(&argc, &argv);

            indicator = app_indicator_new(
                "battery-charge-threshold-tray-applet",
                CAPPED_ICON,
                APP_INDICATOR_CATEGORY_HARDWARE);
            app_indicator_set_status(indicator, APP_INDICATOR_STATUS_ACTIVE);

            GtkWidget *menu = gtk_menu_new();
            cap_item = gtk_check_menu_item_new_with_label("Cap battery charge");
            g_signal_connect(cap_item, "toggled", G_CALLBACK(set_charge_cap), NULL);
            gtk_menu_shell_append(GTK_MENU_SHELL(menu), cap_item);

            status_item = gtk_menu_item_new_with_label(" ");
            gtk_widget_set_sensitive(status_item, FALSE);
            gtk_menu_shell_append(GTK_MENU_SHELL(menu), status_item);

            gtk_menu_shell_append(
                GTK_MENU_SHELL(menu), gtk_separator_menu_item_new());
            GtkWidget *quit_item = gtk_menu_item_new_with_label("Quit");
            g_signal_connect(quit_item, "activate", G_CALLBACK(gtk_main_quit), NULL);
            gtk_menu_shell_append(GTK_MENU_SHELL(menu), quit_item);

            gtk_widget_show_all(menu);
            app_indicator_set_menu(indicator, GTK_MENU(menu));

            refresh(NULL);
            g_timeout_add_seconds(30, refresh, NULL);
            gtk_main();
            return 0;
        }
      ''} \
      -o battery-charge-threshold-tray-applet \
      $(pkg-config --cflags --libs ayatana-appindicator3-0.1 gtk+-3.0)
  '';

  installPhase = ''
    icon_dir=$out/share/battery-charge-threshold-tray-applet
    license_dir=$out/share/licenses/battery-charge-threshold-tray-applet

    mkdir -p $out/bin $icon_dir $license_dir
    cp battery-charge-threshold-tray-applet $out/bin/

    cp ${material-design-icons-svg}/svg/battery-charging-90.svg \
      $icon_dir/battery-charge-threshold-full.svg
    cp ${material-design-icons-svg}/svg/battery-charging-50.svg \
      $icon_dir/battery-charge-threshold-capped.svg
    cp ${material-design-icons-svg}/LICENSE $license_dir/MaterialDesign-SVG-LICENSE
    printf '%s\n' \
      'Icons: battery-charging-90.svg and battery-charging-50.svg from Pictogrammers Material Design Icons SVG v7.4.47' \
      'Source: https://github.com/Templarian/MaterialDesign-SVG' \
      'License: Apache License 2.0; see MaterialDesign-SVG-LICENSE' \
      > $license_dir/MaterialDesign-SVG-SOURCE
  '';
};

in {
  assertions = [{
    assertion = config.services.tlp.enable;
    message = "battery-charge-threshold-tray-applet requires services.tlp.enable = true";
  }];

  systemd.user.services.battery-charge-threshold-tray-applet = {
    description = "Battery charge threshold tray applet";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    after = [ "graphical-session.target" ];
    serviceConfig = {
      ExecStart =
        "${battery-charge-threshold-tray-applet}/bin/battery-charge-threshold-tray-applet";
      Restart = "on-failure";
    };
  };

  # Bridge the StatusNotifierItem to legacy XEmbed trays during X11 sessions
  systemd.user.services.battery-charge-threshold-tray-applet-xembed = {
    description = "Battery charge threshold tray applet XEmbed bridge";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    after = [ "graphical-session.target" "battery-charge-threshold-tray-applet.service" ];
    unitConfig.ConditionEnvironment = "XDG_SESSION_TYPE=x11";
    serviceConfig = {
      ExecStart = lib.getExe pkgs.snixembed;
      Restart = "on-failure";
    };
  };
}
