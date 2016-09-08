library("ggplot2")
library("reshape2")
library("gridExtra")

theme_set(theme_bw())

data <- read.table(file="case3.csv", header=T, sep=",")

runtime_data = data[ which(data$result_type == "runtime"), ]

plot_filtered_line_chart <- function(data_src, type, xcol, ycol, title, xlabel, ylabel, color) {
    fdata <- data_src[ which(data_src$type == type), ]
    fdata$xcol <- fdata[,xcol]
    fdata$ycol <- fdata[,ycol]
    chart <- ggplot(fdata, aes(x=xcol, y=ycol)) +
        labs(title=title) +
        geom_line(colour=color) +
        geom_point(size=2, shape=21, fill="white", colour=color) +
        xlab(xlabel) +
        ylab(ylabel)
    return (chart)
}

plot_grouped_line_chart <- function(data_src, xcol, ycol, grouped_by, title, xlabel, ylabel, legend_title, colors) {
    data_src$xcol <- data_src[,xcol]
    data_src$ycol <- data_src[,ycol]
    data_src$grouped_by <- data_src[,grouped_by]

    chart <- ggplot(data_src, aes(x=xcol, y=ycol, log="y", colour=grouped_by, group=grouped_by)) +
        theme(legend.position="top") +
        geom_line() +
        geom_point(size=2, shape=21, fill="white") +
        xlab(xlabel) +
        ylab(ylabel) +
        labs(title=title) +
        scale_colour_manual(name=legend_title, values=colors)

    return(chart)
}

colors <- c("#BF4717", "#608FE2", "#F4E005")

native_vs_nonnative_chart <- plot_grouped_line_chart(runtime_data, "existing_events_n", "value", "type",
    "(a) native event type vs triple-based conditions (events N=1000)",
    "# of existing events",
    "avg. matching time (ms)",
    "Event condition method",
    colors)

activation_data = subset(data, result_type == "activation" & label != "total")

native_activation_data = subset(activation_data, type == "native")
nonnative_activation_data = subset(activation_data, type == "non-native")

activation_colors <- c("#BF4717", "#608FE2", "#F4E005", "#3799DE")

native_activation_chart <- plot_grouped_line_chart(native_activation_data, "existing_events_n", "value", "label",
    "(b) node activations (events N=1000, native event method)",
    "# of existing events",
    "# of activations",
    "Node activated",
    activation_colors)

nonnative_activation_chart <- plot_grouped_line_chart(nonnative_activation_data, "existing_events_n", "value", "label",
    "(c) node activations (events N=1000, non-native event method)",
    "# of existing events",
    "# of activations",
    "Node activated",
    activation_colors)

pdf(file="case3.pdf")
grid.arrange(native_vs_nonnative_chart,
             native_activation_chart,
             nonnative_activation_chart,
             ncol=1)
dev.off()
