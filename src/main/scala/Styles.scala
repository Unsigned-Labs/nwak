import calico.html.io.*

object Styles {
  val button = cls :=
    "cursor-pointer bg-primary-600 hover:bg-primary-500 text-white font-semibold py-2.5 px-5 rounded-lg transition-all shadow-lg shadow-primary-900/50 hover:shadow-primary-800/60 hover:-translate-y-0.5"
  val buttonSmall = cls :=
    "cursor-pointer text-sm bg-primary-600 hover:bg-primary-500 text-white font-semibold px-3 py-1.5 rounded-md transition-all shadow-md shadow-primary-900/50 hover:shadow-primary-800/60 hover:-translate-y-0.5"
  val card = cls :=
    "bg-slate-800/50 border border-slate-700/50 rounded-xl p-4 shadow-lg hover:shadow-xl hover:border-slate-600/50 transition-all min-w-[300px]"
  val cardTitle = cls :=
    "text-base font-bold text-slate-200 mb-3 pb-2 border-b border-slate-700/50"
  val iconContainer = cls :=
    "flex-shrink-0 w-12 h-12 rounded-full bg-primary-900/40 border border-primary-800/50 flex items-center justify-center transition-all hover:bg-primary-800/40 hover:border-primary-700/50"
  val sectionLabel = cls :=
    "text-xs font-semibold text-slate-400 uppercase tracking-wide mb-2"
  val buttonGroup = cls :=
    "flex flex-col gap-2"
  val toolbar = cls :=
    "flex gap-2 justify-end items-center px-1"
  val actionDescription = cls :=
    "text-xs text-slate-500 mt-1"
  val toolbarButton = cls :=
    "cursor-pointer text-sm bg-slate-800 hover:bg-slate-700 border border-slate-700 hover:border-slate-600 text-slate-300 hover:text-slate-100 font-medium px-3 py-1.5 rounded-md transition-all shadow-sm hover:shadow-md"
  val buttonWithIcon = cls :=
    "cursor-pointer bg-primary-600 hover:bg-primary-500 text-white font-semibold py-2.5 px-5 rounded-lg transition-all shadow-lg shadow-primary-900/50 hover:shadow-primary-800/60 hover:-translate-y-0.5 flex items-center justify-center gap-2"
}
