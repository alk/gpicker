#include <stdio.h>
#include <stdlib.h>
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include <glade/glade.h>
#include <sys/types.h>
#include <unistd.h>
#include <memory.h>
#include <fcntl.h>
#include <errno.h>
#include <time.h>
#include "scorer.h"

#define max(a,b) ({__typeof__ (a) ____a = (a); __typeof__ (b) ____b = (b); ____b > ____a ? ____b : ____a; })

__attribute__((noreturn))
static
void memory_exhausted(void)
{
	fputs("memory exhausted", stderr);
	abort();
}

struct vector {
	char *buffer;
	int eltsize;
	int used;
	int avail;
};

void *vector_append(struct vector *v)
{
	if (v->used == v->avail) {
		int new_avail = v->avail * 2;
		if (!new_avail)
			new_avail = 4096 / v->eltsize;
		v->buffer = realloc(v->buffer, new_avail * v->eltsize);
		v->avail = new_avail;
	}

	return v->buffer + v->eltsize * (v->used++);
}

void vector_clear(struct vector *v)
{
	v->used = 0;
	v->buffer = realloc(v->buffer, 0);
	v->avail = 0;
}

static
void finish_timing(clock_t start, char *info)
{
	clock_t ticks = clock() - start;
	double msecs = ticks/(double)CLOCKS_PER_SEC*1000.0;
	printf("%s took %g msecs\n", info, msecs);
}

struct filename {
	char *p;
	int dirlength;
};

static GladeXML *glade_ui;
static GtkWindow *top_window;
static GtkEntry *name_entry;
static GtkTreeView *tree_view;
static GtkListStore *list_store;

struct vector files_vector = {.eltsize = sizeof(struct filename)};

#define nfiles (files_vector.used)
#define files ((struct filename *)(files_vector.buffer))

static
void add_filename(char *p, int dirlength)
{
	struct filename *last = vector_append(&files_vector);

	last->p = p;
	last->dirlength = dirlength;
}

#define INIT_BUFSIZE (128*1024)
#define MIN_BUFSIZE_FREE 32768

static
char *input_names(int fd, char **endp)
{
	int bufsize = INIT_BUFSIZE;
	char *buf = malloc(bufsize);
	int filled = 0;

	if (!buf)
		memory_exhausted();

	do {
		int readen = read(fd, buf+filled, bufsize-filled);
		if (readen == 0)
			break;
		else if (readen < 0) {
			if (errno == EINTR)
				continue;
			perror("read_names");
			break;
		}
		filled += readen;
		if (bufsize - filled < MIN_BUFSIZE_FREE) {
			bufsize = filled + MIN_BUFSIZE_FREE;
			buf = realloc(buf, bufsize);
			if (!buf)
				memory_exhausted();
		}
	} while (1);
	if (filled && buf[filled-1])
		filled++;
	buf = realloc(buf, filled);
	if (!buf)
		memory_exhausted();
	buf[filled-1] = 0;
	*endp = buf+filled;
	return buf;
}

int filter_filename(struct filename *name,
		    const void *_pattern,
		    struct filter_result *result,
		    unsigned *ematch)
{
	const char *pattern = _pattern;
	int patlen = strlen(pattern);
	unsigned match[patlen];
	int score = score_simple_string(name->p + name->dirlength, pattern, match);
	if (score < 0)
		return 0;
	result->score = score;
	result->last_match_pos = (patlen > 0) ? match[patlen-1] : 0;
	if (ematch) {
		int i;
		for (i=0;i<patlen;i++)
			ematch[i] = match[i] + name->dirlength;
	}
	return 1;
}

struct split_pattern {
	char *basename;
	char *dirname;
};

int filter_filename_with_dir(struct filename *name,
			     const void *_pattern,
			     struct filter_result *result,
			     unsigned *ematch)
{
	const struct split_pattern *pattern = (struct split_pattern *)_pattern;
	int baselen = strlen(pattern->basename);
	int namelen = strlen(name->p);
	int dirlen = strlen(pattern->dirname);
	unsigned base_match[baselen];
	unsigned dir_match[dirlen];
	struct scorer_query qry;

	if (!name->dirlength)
		return 0;

	qry.pattern = pattern->basename;
	qry.right_match = 0;
	result->score = score_string(name->p+name->dirlength, &qry, namelen-name->dirlength, base_match);
	if (result->score < 0)
		return 0;
	result->last_match_pos = baselen ? base_match[baselen-1] : 0;
	qry.pattern = pattern->dirname;
	qry.right_match = 1;
	result->dirscore = score_string(name->p, &qry, name->dirlength-1, dir_match);
	if (result->dirscore < 0)
		return 0;
	result->first_dir_match_pos = (name->dirlength-1) ? dir_match[0] : 0;

	if (ematch) {
		int i;
		int dirlen = strlen(pattern->dirname);
		for (i=0;i<dirlen;i++)
			ematch[i] = dir_match[i];
		ematch[dirlen] = name->dirlength-1;
		for (i=0;i<baselen;i++)
			ematch[i+dirlen+1] = base_match[i]+name->dirlength;
	}
	
	return 1;
}

void destroy_filter_with_dir(const void *data)
{
	struct split_pattern *pattern = (struct split_pattern *)data;
	free(pattern->basename);
	free(pattern->dirname);
	free(pattern);
}

typedef void (*filter_destructor)(const void *);
typedef int (*filter_func)(struct filename *, const void *, struct filter_result *, unsigned *);

const void *prepare_filter(const char *filter, filter_func *func, filter_destructor *destructor)
{
	char *last_slash = strrchr(filter, '/');
	if (!last_slash) {
		*destructor = NULL;
		*func = filter_filename;
		return filter;
	} else {
		*destructor = destroy_filter_with_dir;
		*func = filter_filename_with_dir;
		struct split_pattern *pat = malloc(sizeof(struct split_pattern));
		pat->basename = strdup(last_slash+1);
		pat->dirname = g_strndup(filter, last_slash-filter);
		return pat;
	}
}

struct vector filtered = {.eltsize = sizeof(struct filter_result)};

static
int compare_filter_result(struct filter_result *a, struct filter_result *b)
{
	int rv = b->score - a->score;
	struct filename *filea, *fileb;
	if (rv)
		return rv;
	rv = b->dirscore - a->dirscore;
	if (rv)
		return rv;
	rv = a->last_match_pos - b->last_match_pos;
	if (rv)
		return rv;
	rv = b->first_dir_match_pos - a->first_dir_match_pos;
	if (rv)
		return rv;
	filea = files + a->index;
	fileb = files + b->index;
	rv = strlen(filea->p+filea->dirlength) - strlen(fileb->p+fileb->dirlength);
	if (rv)
		return rv;
	return filea->dirlength - fileb->dirlength;
}

static
void filter_files(char *pattern)
{
	int i;
	struct filter_result result;
	struct filter_result *results;
	GtkTreeIter iter;
	clock_t start;
	const void *filter;
	filter_func filter_func;
	filter_destructor destructor = 0;

	filter = prepare_filter(pattern, &filter_func, &destructor);

	vector_clear(&filtered);

	for (i=0; i<nfiles; i++) {
		int passes = filter_func(files + i, filter, &result, 0);
		struct filter_result *place;
		if (!passes)
			continue;
		place = vector_append(&filtered);
		result.index = i;
		*place = result;
	}

	if (destructor)
		destructor(filter);

	results = (struct filter_result *)filtered.buffer;
	qsort(results, filtered.used, sizeof(struct filter_result), (int (*)(const void *, const void *))compare_filter_result);

	start = clock();

	g_object_ref(G_OBJECT(list_store));
	gtk_tree_view_set_model(tree_view, 0);

	gtk_list_store_clear(list_store);

	finish_timing(start, "gtk_list_store_clear");
	start = clock();

	for (i=0; i<filtered.used; i++) {
		gtk_list_store_append(list_store, &iter);
		gtk_list_store_set(list_store, &iter,
				   0, results[i].index,
				   -1);
	}

	finish_timing(start, "adding filtered data");
	start = clock();

	gtk_tree_view_set_model(tree_view, GTK_TREE_MODEL(list_store));
	g_object_unref(G_OBJECT(list_store));

	finish_timing(start, "setting model back");

	{
		GtkTreeSelection *sel = gtk_tree_view_get_selection(tree_view);
		GtkTreePath *path = gtk_tree_path_new_from_indices(0,-1);
		gtk_tree_selection_select_path(sel, path);
		gtk_tree_path_free(path);
	}
}

static
int filename_compare(struct filename *a, struct filename *b)
{
	return strcasecmp(a->p, b->p);
}

static
void read_filenames(int fd)
{
	char *endp;
	char *buf = input_names(fd, &endp);
	char *p = buf;

	while (p < endp) {
		int dirlength = 0;
		char *start = p;
		char ch;
		while ((ch = *p++))
			if (ch == '/')
				dirlength = p - start;
		add_filename(start, dirlength);
	}

	qsort(files, nfiles, sizeof(struct filename), (int (*)(const void *, const void *))filename_compare);
}

static
void cell_data_func(GtkTreeViewColumn *col,
		    GtkCellRenderer *_renderer,
		    GtkTreeModel *model,
		    GtkTreeIter *iter,
		    gpointer dummy)
{
	GtkCellRendererText *renderer = GTK_CELL_RENDERER_TEXT(_renderer);
	int index;
	char *text=0;
	gtk_tree_model_get(model, iter, 0, &index, -1);
	text = files[index].p;
	if (text) {
		const char *pattern = gtk_entry_get_text(GTK_ENTRY(name_entry));
		int patlen = strlen(pattern);
		unsigned match[patlen];
		const void *filter;
		filter_func filter_func;
		filter_destructor destructor = 0;
		struct filter_result result;
		int passes;
		int i;

		g_object_set(G_OBJECT(renderer),
			     "text", text,
			     NULL);

		filter = prepare_filter(pattern, &filter_func, &destructor);
		passes = filter_func(files + index, filter, &result, match);
		if (destructor)
			destructor(filter);
		if (!passes)
			return;

		PangoAttrList *list = pango_attr_list_new();
		for (i=0;i<patlen;i++) {
			PangoAttribute *attr = pango_attr_weight_new(PANGO_WEIGHT_BOLD);
			attr->start_index = match[i];
			attr->end_index = match[i]+1;
			pango_attr_list_insert(list, attr);
		}

		g_object_set(G_OBJECT(renderer),
			     "attributes", list,
			     NULL);
		pango_attr_list_unref(list);
	}
}

static
void setup_column(void)
{
	GtkCellRenderer *renderer = gtk_cell_renderer_text_new();
	GtkTreeViewColumn *col;
	g_object_set(G_OBJECT(renderer),
		     "ellipsize", PANGO_ELLIPSIZE_START,
		     "alignment", PANGO_ALIGN_RIGHT,
		     NULL);

	col = gtk_tree_view_column_new_with_attributes("filename", renderer, NULL);

	gtk_tree_view_column_set_cell_data_func(col, renderer, cell_data_func, 0, 0);
	gtk_tree_view_column_set_sizing(col, GTK_TREE_VIEW_COLUMN_FIXED);

	gtk_tree_view_append_column(tree_view, col);
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
void on_entry_changed(GtkEditable *editable,
		      gpointer data)
{
	clock_t start = clock();

	char *text = g_strdup(gtk_entry_get_text(GTK_ENTRY(editable)));
	filter_files(text);
	free(text);

	finish_timing(start, "filtration");
}

static
void setup_data(void)
{
	list_store = gtk_list_store_new(1, G_TYPE_INT);
	gtk_tree_view_set_model(tree_view, GTK_TREE_MODEL(list_store));

	{
		FILE *pipe = popen("(cd /root/src/altoros/phase1; find -type f -print0)","r");
		read_filenames(fileno(pipe));
		fclose(pipe);
	}

	setup_column();

	on_entry_changed(GTK_EDITABLE(name_entry), 0);
}

static
gboolean key_events_delegator(GtkWidget *w, GdkEventKey *e, gpointer dummy)
{
	gint sel_start, sel_end;
	gboolean had_selection;
	gint old_cursor_pos;
	gboolean rv;

	had_selection = gtk_editable_get_selection_bounds(GTK_EDITABLE(name_entry),
							  &sel_start, &sel_end);
	old_cursor_pos = gtk_editable_get_position(GTK_EDITABLE(name_entry));

	g_object_set(G_OBJECT(tree_view), "can-focus", TRUE, NULL);
	gtk_widget_grab_focus(GTK_WIDGET(tree_view));
	rv = gtk_widget_event(GTK_WIDGET(tree_view), (GdkEvent *)e);
	gtk_widget_grab_focus(GTK_WIDGET(name_entry));
	g_object_set(G_OBJECT(tree_view), "can-focus", FALSE, NULL);

	gtk_editable_set_position(GTK_EDITABLE(name_entry), old_cursor_pos);
	if (had_selection)
		gtk_editable_select_region(GTK_EDITABLE(name_entry), sel_start, sel_end);
	else
		gtk_editable_select_region(GTK_EDITABLE(name_entry), old_cursor_pos, old_cursor_pos);

	return rv;
}

static
void setup_signals(void)
{
	g_signal_connect(top_window, "destroy", G_CALLBACK(exit_program), 0);
	g_signal_connect(top_window, "key-press-event", G_CALLBACK(on_top_window_keypress), 0);
	g_signal_connect(name_entry, "activate", G_CALLBACK(choice_made), 0);
	g_signal_connect(tree_view, "row-activated", G_CALLBACK(choice_made), 0);
	g_signal_connect(name_entry, "changed", G_CALLBACK(on_entry_changed), 0);

	g_object_set(G_OBJECT(tree_view), "can-focus", FALSE, NULL);
	g_signal_connect_after(name_entry, "key-press-event", G_CALLBACK(key_events_delegator), 0);
	g_signal_connect_after(name_entry, "key-release-event", G_CALLBACK(key_events_delegator), 0);
}

int main(int argc, char **argv)
{
	clock_t tstart = clock();

	gtk_init(&argc, &argv);
	glade_ui = glade_xml_new("filechooser.glade", 0, 0);

	finish_timing(tstart, "gtk initialization");

	tstart = clock();

	top_window = GTK_WINDOW(glade_xml_get_widget(glade_ui, "top-window"));
	tree_view = GTK_TREE_VIEW(glade_xml_get_widget(glade_ui, "treeview"));
	name_entry = GTK_ENTRY(glade_xml_get_widget(glade_ui, "name-entry"));

	finish_timing(tstart, "init2");
	tstart = clock();

	gtk_widget_show_all(GTK_WIDGET(top_window));
	while (gtk_events_pending())
		gtk_main_iteration();

	finish_timing(tstart, "initial show");
	tstart = clock();

	setup_signals();

	finish_timing(tstart, "setup_signals");
	tstart = clock();

	setup_data();

	finish_timing(tstart, "setup_data");

	gtk_main();
	return 0;
}
