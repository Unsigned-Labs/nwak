import calico.html.io.*

object Styles {
  val button = cls :=
    "cursor-pointer bg-primary-600 hover:bg-primary-500 text-white font-semibold py-2.5 px-5 rounded-lg transition-all shadow-lg shadow-primary-900/50 hover:shadow-primary-800/60 hover:-translate-y-0.5"
  val buttonSmall = cls :=
    "cursor-pointer text-sm bg-primary-600 hover:bg-primary-500 text-white font-semibold px-3 py-1.5 rounded-md transition-all shadow-md shadow-primary-900/50 hover:shadow-primary-800/60 hover:-translate-y-0.5"
  val card = cls :=
    "bg-slate-800/50 border border-slate-700/50 rounded-xl p-4 shadow-lg hover:shadow-xl hover:border-slate-600/50 transition-all"
  val cardTitle = cls :=
    "text-base font-bold text-slate-200 mb-3 pb-2 border-b border-slate-700/50"
}
