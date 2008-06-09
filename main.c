#include <stdio.h>
#include <stdlib.h>
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include <glade/glade.h>

GladeXML *glade_ui;
GtkWindow *top_window;
GtkEntry *name_entry;
GtkTreeView *tree_view;
GtkListStore *list_store;

static
void setup_data(void)
{
	list_store = gtk_list_store_new(1, G_TYPE_STRING);
	gtk_tree_view_set_model(tree_view, GTK_TREE_MODEL(list_store));
}

static
void exit_program(void)
{
	gtk_main_quit();
}

static
void choice_made(void)
{
	printf("Choice made!\n");
	exit_program();
}

static
gboolean on_top_window_keypress(GtkWidget *_dummy,
				GdkEventKey *event,
				gpointer _dummy2)
{
	if (event->keyval == GDK_Escape) {
		exit_program();
		return TRUE;
	}
	return FALSE;
}

static
void setup_signals(void)
{
	g_signal_connect(top_window, "destroy", G_CALLBACK(exit_program), 0);
	g_signal_connect(top_window, "key-press-event", G_CALLBACK(on_top_window_keypress), 0);
	g_signal_connect(name_entry, "activate", G_CALLBACK(choice_made), 0);
	g_signal_connect(tree_view, "row-activated", G_CALLBACK(choice_made), 0);
}

int main(int argc, char **argv)
{
	gtk_init(&argc, &argv);
	glade_ui = glade_xml_new("filechooser.glade", 0, 0);

	top_window = GTK_WINDOW(glade_xml_get_widget(glade_ui, "top-window"));
	tree_view = GTK_TREE_VIEW(glade_xml_get_widget(glade_ui, "treeview"));
	name_entry = GTK_ENTRY(glade_xml_get_widget(glade_ui, "name-entry"));

	gtk_widget_show_all(GTK_WIDGET(top_window));
	while (gtk_events_pending())
		gtk_main_iteration();

	setup_signals();

	setup_data();

	gtk_main();
	return 0;
}
