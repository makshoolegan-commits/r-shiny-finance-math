# Интерактивный сайт из трёх блоков задач на языке R.
# Для запуска: install.packages("shiny") (один раз), затем shiny::runApp()

library(shiny)

# ---------- Вспомогательные функции ----------
number_ru <- function(x, digits = 2) {
  formatC(x, format = "f", digits = digits, big.mark = " ", decimal.mark = ",")
}

money_ru <- function(x) paste0(number_ru(x), " ₽")

metric <- function(label, value, note = NULL) {
  tags$div(
    class = "metric",
    tags$div(class = "metric-label", label),
    tags$div(class = "metric-value", value),
    if (!is.null(note)) tags$div(class = "metric-note", note)
  )
}

section_heading <- function(tag, title, description) {
  tags$div(
    class = "section-heading",
    tags$span(class = "section-tag", tag),
    tags$h2(title),
    tags$p(description)
  )
}

panel <- function(...) tags$div(class = "panel", ...)

# Проверяем математический ввод: разрешены только числа, x, pi и обычные
# математические операции/функции. Это важно для приложения в интернете.
parse_safe_math <- function(source) {
  if (length(source) != 1L || is.na(source) || nchar(source) > 120L) {
    stop("Введите короткую математическую функцию (до 120 символов).")
  }
  exprs <- tryCatch(parse(text = source, keep.source = FALSE),
                    error = function(e) stop("Не удалось прочитать функцию. Проверьте скобки и знаки операций."))
  if (length(exprs) != 1L) stop("Можно ввести только одну функцию.")

  count <- 0L
  check <- function(node, depth = 0L) {
    count <<- count + 1L
    if (depth > 25L || count > 100L) stop("Слишком сложное выражение.")
    if (is.numeric(node)) {
      if (length(node) != 1L || !is.finite(node) || abs(node) > 1e7) {
        stop("Используйте конечные числа умеренной величины.")
      }
      return(invisible(NULL))
    }
    if (is.name(node)) {
      if (!(as.character(node) %in% c("x", "pi"))) {
        stop("Допустимы только переменная x и константа pi.")
      }
      return(invisible(NULL))
    }
    if (!is.call(node) || !is.name(node[[1L]])) {
      stop("Выражение содержит недопустимую конструкцию.")
    }
    fun <- as.character(node[[1L]])
    args <- as.list(node)[-1L]
    if (!(fun %in% c("+", "-", "*", "/", "^", "(",
                    "sin", "cos", "tan", "exp", "log", "sqrt"))) {
      stop("Допустимы +, -, *, /, ^, sin, cos, tan, exp, log, sqrt.")
    }
    if (fun %in% c("+", "-")) {
      if (!(length(args) %in% c(1L, 2L))) stop("Неправильное число аргументов.")
    } else if (fun %in% c("*", "/", "^")) {
      if (length(args) != 2L) stop("Неправильное число аргументов.")
    } else if (length(args) != 1L) {
      stop("Функция должна иметь один аргумент.")
    }
    if (fun == "^" && is.numeric(args[[2L]]) && abs(args[[2L]]) > 30) {
      stop("Показатель степени должен быть не больше 30 по модулю.")
    }
    for (arg in args) check(arg, depth + 1L)
    invisible(NULL)
  }
  check(exprs[[1L]])
  exprs[[1L]]
}

math_value <- function(expression, x) {
  safe_functions <- list(
    "+" = base::`+`, "-" = base::`-`, "*" = base::`*`,
    "/" = base::`/`, "^" = base::`^`, "(" = base::`(`,
    sin = base::sin, cos = base::cos, tan = base::tan,
    exp = base::exp, log = base::log, sqrt = base::sqrt,
    x = x, pi = base::pi
  )
  env <- list2env(safe_functions, parent = emptyenv())
  suppressWarnings(eval(expression, envir = env))
}

# ---------- Оформление ----------
style <- "
  :root { --bg:#f5f7fb; --ink:#192436; --muted:#65758c;
          --edge:#e6ebf3; --blue:#4358df; --violet:#7354cc;
          --green:#109c7d; }
  html, body { background:var(--bg); color:var(--ink); font-family:
    -apple-system, BlinkMacSystemFont, 'Segoe UI', Arial, sans-serif; }
  .container-fluid { padding:0; }
  .app-shell { max-width:1250px; margin:auto; padding:24px 25px 65px; }
  .hero { color:white; padding:41px 45px; border-radius:24px;
    background:linear-gradient(118deg,#192544 0%,#3148a1 58%,#7050ae 100%);
    box-shadow:0 18px 45px rgba(32,43,98,.15); }
  .eyebrow { font-size:12px; font-weight:750; letter-spacing:2px;
    color:#b9d4ff; margin-bottom:13px; }
  .hero h1 { font-size:38px; font-weight:800; line-height:1.17;
    margin:0 0 10px; letter-spacing:-.6px; }
  .hero p { color:#e1e8ff; font-size:16px; line-height:1.6;
    max-width:780px; margin:0; }
  .cards { display:grid; grid-template-columns:repeat(3,1fr);
    gap:17px; margin:22px 0 28px; }
  .nav-card { background:white; border:1px solid var(--edge);
    border-radius:18px; padding:23px; box-shadow:0 5px 21px rgba(39,52,80,.04);
    display:flex; flex-direction:column; min-height:212px; }
  .nav-icon { width:48px; height:48px; border-radius:14px; display:flex;
    justify-content:center; align-items:center; font-size:23px;
    font-weight:750; margin-bottom:13px; }
  .nav-card.finance .nav-icon { background:#e0f7ed; color:#0d9876; }
  .nav-card.deriv .nav-icon { background:#e7ebff; color:#465ce3; }
  .nav-card.stats .nav-icon { background:#f3eaff; color:#8454c8; }
  .nav-card h3 { margin:0 0 7px; font-size:20px; font-weight:760; }
  .nav-card p { color:var(--muted); font-size:14px; flex:1;
    line-height:1.55; margin-bottom:15px; }
  .go-btn, .go-btn:focus { background:none; border:0; padding:0;
    color:#3c51ce; font-size:14px; text-align:left; font-weight:750;
    box-shadow:none; outline:0; }
  .go-btn:hover { color:#2739ae; background:none; border:none; }
  .work-area { background:white; border:1px solid var(--edge);
    border-radius:22px; padding:27px 29px 31px; box-shadow:0 5px 25px rgba(36,47,70,.04); }
  .section-heading { border-bottom:1px solid var(--edge);
    padding-bottom:17px; margin-bottom:23px; }
  .section-tag { display:inline-block; font-size:11px; font-weight:800;
    letter-spacing:1.3px; color:#5c67b7; text-transform:uppercase; }
  .section-heading h2 { font-size:27px; font-weight:800; margin:7px 0; }
  .section-heading p { margin:0; color:var(--muted); line-height:1.5; }
  .panel { background:#fafbfe; border:1px solid var(--edge);
    border-radius:15px; padding:19px; margin-bottom:18px; }
  .panel h3 { margin:0 0 17px; font-size:17px; font-weight:760; }
  .form-control, .selectize-input { border:1px solid #dce3ef;
    border-radius:9px; box-shadow:none; }
  .form-control:focus { border-color:#8795e9; box-shadow:0 0 0 3px #e9ecff; }
  .control-label { font-weight:650; color:#354258; }
  .metric-grid { display:grid; grid-template-columns:repeat(3,minmax(0,1fr));
    gap:11px; margin:0 0 16px; }
  .metric { border:1px solid #e5ebfa; background:#f7f9ff;
    border-radius:12px; padding:15px 16px; min-height:115px; }
  .metric-label { color:#66748d; font-size:13px; margin-bottom:10px; }
  .metric-value { font-size:21px; font-weight:810; overflow-wrap:anywhere;
    color:#263b83; }
  .metric-note { margin-top:6px; color:#76859b; font-size:12px; }
  .chart-box { margin-top:14px; padding:10px 5px 0; background:white;
    border:1px solid var(--edge); border-radius:13px; }
  .chart-box h3 { margin:11px 15px 0; font-size:16px; font-weight:750; }
  .table>thead>tr>th { background:#eef2fc; color:#354669; border:0; }
  .table>tbody>tr>td { border-color:#edf0f7; }
  .help-note { background:#edf4ff; color:#3a567f; border-radius:12px;
    padding:13px 16px; font-size:13px; line-height:1.6; margin-top:10px; }
  .math-result { background:#edf4ff; border:1px solid #d9e5ff;
    color:#223a7c; border-radius:13px; padding:18px; font-size:17px;
    line-height:1.9; overflow-wrap:anywhere; }
  .footer { color:#8a97aa; font-size:12px; margin:23px 4px 0; text-align:center; }
  @media (max-width:900px) {
    .cards { grid-template-columns:1fr; }
    .nav-card { min-height:unset; }
    .metric-grid { grid-template-columns:1fr; }
    .hero { padding:30px 26px; }
    .hero h1 { font-size:29px; }
    .app-shell { padding:12px 12px 38px; }
    .work-area { padding:19px 15px; }
  }
"

# ---------- Интерфейс ----------
ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$title("Практикум по R — три блока задач"),
    tags$style(HTML(style))
  ),
  tags$div(class = "app-shell",
    tags$header(class = "hero",
      tags$div(class = "eyebrow", "R • SHINY • ИНТЕРАКТИВНЫЕ ЗАДАЧИ"),
      tags$h1("Математика и финансы на R"),
      tags$p("Три практических раздела: финансовые вычисления, производные и статистический анализ. Выбирайте задачу, вводите данные и изучайте результат на графике.")
    ),
    tags$div(class = "cards",
      tags$div(class = "nav-card finance",
        tags$div(class = "nav-icon", "₽"),
        tags$h3("Финансовые задачи"),
        tags$p("Сложные проценты, кредиты и чистая приведённая стоимость."),
        actionButton("go_fin", "Открыть раздел →", class = "go-btn")
      ),
      tags$div(class = "nav-card deriv",
        tags$div(class = "nav-icon", "f′"),
        tags$h3("Производные"),
        tags$p("Аналитическая производная, значение в точке и касательная."),
        actionButton("go_der", "Открыть раздел →", class = "go-btn")
      ),
      tags$div(class = "nav-card stats",
        tags$div(class = "nav-icon", "σ"),
        tags$h3("Статистика"),
        tags$p("Среднее, медиана, разброс данных, доверительный интервал и графики."),
        actionButton("go_stat", "Открыть раздел →", class = "go-btn")
      )
    ),
    tags$main(class = "work-area",
      tabsetPanel(id = "area", type = "hidden", selected = "finance",
        tabPanelBody("finance",
          section_heading("01 / ФИНАНСЫ", "Финансовые задачи",
                          "Выберите тип расчёта, измените исходные данные и посмотрите решение."),
          fluidRow(
            column(4,
              panel(
                tags$h3("Условие задачи"),
                selectInput("fin_task", "Что рассчитать?",
                  choices = c("Сложные проценты по вкладу" = "deposit",
                              "Аннуитетный кредит" = "loan",
                              "Чистая приведённая стоимость (NPV)" = "npv")),
                conditionalPanel("input.fin_task == 'deposit'",
                  numericInput("dep_sum", "Начальный вклад, ₽", 100000, min = 1, max = 1e9),
                  numericInput("dep_rate", "Годовая ставка, %", 12, min = 0, max = 100, step = 0.5),
                  numericInput("dep_years", "Срок, лет", 5, min = 1, max = 40, step = 1),
                  selectInput("dep_freq", "Начисление процентов",
                    choices = c("Ежегодно" = 1, "Ежеквартально" = 4, "Ежемесячно" = 12),
                    selected = 12)
                ),
                conditionalPanel("input.fin_task == 'loan'",
                  numericInput("loan_sum", "Размер кредита, ₽", 1000000, min = 1, max = 1e9),
                  numericInput("loan_rate", "Годовая ставка, %", 14, min = 0, max = 100, step = 0.5),
                  numericInput("loan_months", "Срок, месяцев", 36, min = 1, max = 480, step = 1)
                ),
                conditionalPanel("input.fin_task == 'npv'",
                  numericInput("npv_initial", "Первоначальные инвестиции, ₽", 100000, min = 1, max = 1e9),
                  numericInput("npv_rate", "Ставка дисконтирования, %", 10, min = 0, max = 100, step = 0.5),
                  numericInput("npv_c1", "Поток в 1-й год, ₽", 45000, min = -1e9, max = 1e9),
                  numericInput("npv_c2", "Поток во 2-й год, ₽", 55000, min = -1e9, max = 1e9),
                  numericInput("npv_c3", "Поток в 3-й год, ₽", 65000, min = -1e9, max = 1e9)
                )
              )
            ),
            column(8,
              tags$h3("Результат решения"),
              uiOutput("fin_metrics"),
              tags$div(class = "help-note", textOutput("fin_explanation")),
              tags$div(class = "chart-box", tags$h3("Визуализация"), plotOutput("fin_plot", height = "300px")),
              tags$h3("Таблица расчёта"),
              tableOutput("fin_table"),
              tags$p(class = "help-note", textOutput("fin_table_note"))
            )
          )
        ),
        tabPanelBody("derivative",
          section_heading("02 / МАТЕМАТИКА", "Производные функций",
                          "Введите f(x), укажите точку x₀ и получите аналитическую производную и касательную."),
          fluidRow(
            column(4,
              panel(
                tags$h3("Условие задачи"),
                selectInput("der_example", "Готовый пример",
                  choices = c("Квадратичная функция" = "x^2 + 3*x - 5",
                              "Кубическая функция" = "x^3 - 4*x",
                              "Синус" = "sin(x)",
                              "Показательная функция" = "exp(x)",
                              "Логарифм" = "log(x)",
                              "Произведение функций" = "x^2 * sin(x)",
                              "Рациональная функция" = "1/(x+2)")),
                textInput("der_function", "Функция f(x)", value = "x^2 + 3*x - 5"),
                numericInput("der_point", "Точка x₀", value = 2, min = -100, max = 100, step = 0.5),
                tags$div(class = "help-note",
                  "Записывайте умножение через * и степени через ^. Пример: x^2 + 3*x. Доступны sin, cos, tan, exp, log, sqrt.")
              )
            ),
            column(8,
              tags$h3("Решение"),
              uiOutput("der_solution"),
              tags$div(class = "chart-box", tags$h3("График функции и касательной"),
                plotOutput("der_plot", height = "370px")),
              tags$div(class = "help-note", "Синяя линия — функция f(x), зелёный пунктир — касательная в выбранной точке. При недопустимом значении (например, log(x) при x ≤ 0) появится сообщение об ошибке.")
            )
          )
        ),
        tabPanelBody("statistics",
          section_heading("03 / АНАЛИЗ ДАННЫХ", "Статистика",
                          "Введите числовую выборку и получите её основные характеристики и график."),
          fluidRow(
            column(4,
              panel(
                tags$h3("Исходные данные"),
                textAreaInput("stat_values", "Числовая выборка",
                  value = "12, 14, 15, 15, 17, 18, 18, 19, 21, 23, 25, 28",
                  rows = 7, width = "100%"),
                selectInput("stat_chart", "Тип графика",
                  choices = c("Гистограмма" = "hist", "Диаграмма размаха" = "box")),
                sliderInput("stat_bins", "Число интервалов гистограммы",
                  min = 3, max = 25, value = 8, step = 1),
                tags$div(class = "help-note",
                  "Разделяйте числа пробелом, запятой, точкой с запятой или переносом строки. Десятичный разделитель — точка (например, 12.5). Минимум два наблюдения.")
              )
            ),
            column(8,
              tags$h3("Результаты анализа"),
              uiOutput("stat_metrics"),
              tags$div(class = "chart-box", tags$h3("Распределение данных"),
                plotOutput("stat_plot", height = "340px")),
              tags$h3("Подробные показатели"),
              tableOutput("stat_table"),
              tags$div(class = "help-note", "Для дисперсии и стандартного отклонения используется выборочная формула (деление на n − 1). Интервал для среднего — двусторонний 95% интервал на основе распределения Стьюдента.")
            )
          )
        )
      )
    ),
    tags$div(class = "footer", "Учебный проект • Язык R • Shiny • Все вычисления выполняются в R")
  )
)

# ---------- Логика расчётов ----------
server <- function(input, output, session) {
  observeEvent(input$go_fin, updateTabsetPanel(session, "area", selected = "finance"))
  observeEvent(input$go_der, updateTabsetPanel(session, "area", selected = "derivative"))
  observeEvent(input$go_stat, updateTabsetPanel(session, "area", selected = "statistics"))

  finance <- reactive({
    req(input$fin_task)
    task <- input$fin_task
    if (task == "deposit") {
      req(input$dep_sum, input$dep_rate, input$dep_years, input$dep_freq)
      p <- input$dep_sum
      r <- input$dep_rate / 100
      t <- as.integer(input$dep_years)
      m <- as.integer(input$dep_freq)
      steps <- 0:(t * m)
      balance <- p * (1 + r / m)^steps
      yearly <- 0:t
      table <- data.frame(
        "Год" = yearly,
        "Капитал, ₽" = vapply(p * (1 + r / m)^(yearly * m), money_ru, character(1L)),
        check.names = FALSE
      )
      list(task = task,
           metrics = list(metric("Начальная сумма", money_ru(p)),
                          metric("Итоговая сумма", money_ru(tail(balance, 1))),
                          metric("Начисленные проценты", money_ru(tail(balance, 1) - p))),
           explanation = paste0("Формула: S = P × (1 + r / m)^(m × t), где P — вклад, r — годовая ставка в долях, m — начислений в год, t — срок."),
           x = steps / m, y = balance, table = table,
           note = "Таблица показывает сумму на начало и конец каждого года.")
    } else if (task == "loan") {
      req(input$loan_sum, input$loan_rate, input$loan_months)
      p <- input$loan_sum
      r <- input$loan_rate / 1200
      n <- as.integer(input$loan_months)
      payment <- if (r == 0) p / n else p * r / (1 - (1 + r)^(-n))
      balance <- numeric(n + 1L)
      balance[1L] <- p
      interest <- principal <- numeric(n)
      for (i in seq_len(n)) {
        interest[i] <- balance[i] * r
        principal[i] <- payment - interest[i]
        balance[i + 1L] <- max(0, balance[i] - principal[i])
      }
      table <- data.frame(
        "Месяц" = seq_len(min(n, 12L)),
        "Платёж, ₽" = rep(money_ru(payment), min(n, 12L)),
        "Проценты, ₽" = vapply(head(interest, 12), money_ru, character(1L)),
        "Остаток, ₽" = vapply(head(balance[-1L], 12), money_ru, character(1L)),
        check.names = FALSE
      )
      list(task = task,
           metrics = list(metric("Ежемесячный платёж", money_ru(payment)),
                          metric("Всего выплат", money_ru(payment * n)),
                          metric("Переплата", money_ru(payment * n - p))),
           explanation = "Аннуитет: A = P × i / (1 − (1+i)^(−n)), где i — месячная ставка, n — число платежей. При i = 0 платёж равен P / n.",
           x = 0:n, y = balance, table = table,
           note = "Показаны первые 12 месяцев (или весь срок, если он короче). Без комиссий и досрочных погашений.")
    } else {
      req(input$npv_initial, input$npv_rate,
          input$npv_c1, input$npv_c2, input$npv_c3)
      investment <- input$npv_initial
      r <- input$npv_rate / 100
      flows <- c(input$npv_c1, input$npv_c2, input$npv_c3)
      discounted <- flows / (1 + r)^(1:3)
      npv <- sum(discounted) - investment
      table <- data.frame(
        "Год" = 0:3,
        "Поток, ₽" = vapply(c(-investment, flows), money_ru, character(1L)),
        "Дисконтированный поток, ₽" = vapply(c(-investment, discounted), money_ru, character(1L)),
        check.names = FALSE
      )
      list(task = task,
           metrics = list(metric("NPV", money_ru(npv)),
                          metric("PV будущих потоков", money_ru(sum(discounted))),
                          metric("Индекс доходности", paste0(number_ru(sum(discounted) / investment), " ×"))),
           explanation = "NPV = −I₀ + CF₁/(1+r) + CF₂/(1+r)^2 + CF₃/(1+r)^3. При NPV > 0 проект создаёт стоимость при заданной ставке дисконтирования.",
           x = 0:3, y = c(-investment, discounted), table = table,
           note = "Три прогнозных денежных потока учитываются на конец соответствующего года.")
    }
  })

  output$fin_metrics <- renderUI({
    tags$div(class = "metric-grid", finance()$metrics)
  })
  output$fin_explanation <- renderText(finance()$explanation)
  output$fin_table <- renderTable(finance()$table, striped = TRUE, bordered = FALSE, spacing = "s")
  output$fin_table_note <- renderText(finance()$note)
  output$fin_plot <- renderPlot({
    data <- finance()
    par(mar = c(4.2, 5.5, 2, 1), bg = "white", las = 1)
    if (data$task == "npv") {
      barplot(data$y, names.arg = data$x, col = ifelse(data$y >= 0, "#25aa86", "#e88877"),
              border = NA, xlab = "Год", ylab = "Дисконтированный поток, ₽")
      abline(h = 0, col = "#67748b")
    } else {
      plot(data$x, data$y, type = "n", xlab = if (data$task == "loan") "Месяц" else "Год",
           ylab = "Сумма, ₽", col.axis = "#4a5972")
      grid(col = "#e7eaf2")
      lines(data$x, data$y, col = "#4358df", lwd = 3)
      points(tail(data$x, 1), tail(data$y, 1), col = "#109c7d", pch = 19, cex = 1.3)
    }
  }, res = 100)

  observeEvent(input$der_example, {
    updateTextInput(session, "der_function", value = input$der_example)
  }, ignoreInit = TRUE)

  deriv <- reactive({
    req(input$der_function, input$der_point)
    info <- tryCatch({
      expr <- parse_safe_math(input$der_function)
      derivative <- D(expr, "x")
      x0 <- input$der_point
      y0 <- math_value(expr, x0)
      d0 <- math_value(derivative, x0)
      if (!is.numeric(y0) || length(y0) != 1L || !is.finite(y0) ||
          !is.numeric(d0) || length(d0) != 1L || !is.finite(d0)) {
        stop("В этой точке функция или её производная не определена.")
      }
      list(expr = expr, derivative = derivative, x0 = x0, y0 = y0, d0 = d0)
    }, error = function(e) list(error = conditionMessage(e)))
    info
  })

  output$der_solution <- renderUI({
    data <- deriv()
    validate(need(is.null(data$error), data$error))
    derivative_text <- paste(deparse(data$derivative, width.cutoff = 500L), collapse = "")
    tags$div(class = "math-result",
      tags$div(tags$b("Функция: "), tags$code(input$der_function)),
      tags$div(tags$b("Производная: "), tags$code(paste0("f'(x) = ", derivative_text))),
      tags$div(tags$b("Значение функции: "),
        paste0("f(", number_ru(data$x0), ") = ", number_ru(data$y0))),
      tags$div(tags$b("Значение производной: "),
        paste0("f'(", number_ru(data$x0), ") = ", number_ru(data$d0))),
      tags$div(tags$b("Касательная: "),
        tags$code(paste0("y = ", number_ru(data$y0), " + (", number_ru(data$d0), ") * (x - (", number_ru(data$x0), "))")))
    )
  })

  output$der_plot <- renderPlot({
    data <- deriv()
    validate(need(is.null(data$error), data$error))
    xx <- seq(data$x0 - 4, data$x0 + 4, length.out = 600)
    yy <- tryCatch(math_value(data$expr, xx), error = function(e) rep(NA_real_, length(xx)))
    if (length(yy) == 1L) yy <- rep(yy, length(xx))
    validate(need(length(yy) == length(xx), "Не получилось построить график."))
    yy[!is.finite(yy)] <- NA_real_
    tangent <- data$y0 + data$d0 * (xx - data$x0)
    valid <- yy[is.finite(yy)]
    validate(need(length(valid) > 1L, "Недостаточно точек для построения графика."))
    # Убираем сильные выбросы возле полюсов для читаемости графика.
    limits <- range(c(quantile(valid, c(.02, .98)), data$y0,
                      quantile(tangent, c(.02, .98))), finite = TRUE)
    if (diff(limits) < 1e-10) limits <- limits + c(-1, 1)
    par(mar = c(4.4, 4.7, 2, 1), bg = "white")
    plot(xx, yy, type = "n", xlim = range(xx), ylim = limits,
         xlab = "x", ylab = "y")
    grid(col = "#edf0f6")
    abline(h = 0, v = 0, col = "#a9b4c6", lty = 3)
    lines(xx, yy, col = "#4358df", lwd = 3)
    lines(xx, tangent, col = "#109c7d", lty = 2, lwd = 2)
    points(data$x0, data$y0, pch = 19, cex = 1.5, col = "#e89a34")
    legend("topleft", c("f(x)", "Касательная", "Точка x₀"),
           col = c("#4358df", "#109c7d", "#e89a34"),
           lty = c(1, 2, NA), pch = c(NA, NA, 19), bty = "n", cex = .85)
  }, res = 100)

  values <- reactive({
    req(input$stat_values)
    tokens <- unlist(strsplit(trimws(input$stat_values), "[[:space:],;]+"))
    tokens <- tokens[nzchar(tokens)]
    validate(need(length(tokens) >= 2L, "Введите как минимум два числа."))
    validate(need(length(tokens) <= 50000L, "Допустимо не более 50 000 чисел."))
    numbers <- suppressWarnings(as.numeric(tokens))
    validate(need(all(is.finite(numbers)),
                  "Найдены некорректные значения. Используйте числа и точку для десятичных дробей."))
    numbers
  })

  summary_data <- reactive({
    x <- values()
    n <- length(x)
    avg <- mean(x)
    s <- sd(x)
    delta <- qt(.975, df = n - 1L) * s / sqrt(n)
    list(n = n, mean = avg, median = median(x), sd = s,
         variance = var(x), min = min(x), max = max(x),
         q1 = unname(quantile(x, .25)), q3 = unname(quantile(x, .75)),
         low = avg - delta, high = avg + delta)
  })

  output$stat_metrics <- renderUI({
    s <- summary_data()
    tags$div(class = "metric-grid",
      metric("Число наблюдений", as.character(s$n)),
      metric("Среднее арифметическое", number_ru(s$mean)),
      metric("Медиана", number_ru(s$median)),
      metric("Стандартное отклонение", number_ru(s$sd)),
      metric("Дисперсия", number_ru(s$variance)),
      metric("Диапазон", paste0(number_ru(s$min), " — ", number_ru(s$max)))
    )
  })
  output$stat_table <- renderTable({
    s <- summary_data()
    data.frame(
      "Показатель" = c("Минимум", "Нижний квартиль (Q1)", "Среднее",
                       "Медиана", "Верхний квартиль (Q3)", "Максимум",
                       "Выборочная дисперсия", "Стандартное отклонение",
                       "95% ДИ: нижняя граница", "95% ДИ: верхняя граница"),
      "Значение" = vapply(c(s$min, s$q1, s$mean, s$median,
                            s$q3, s$max, s$variance, s$sd, s$low, s$high),
                          number_ru, character(1L)),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = FALSE, spacing = "s")
  output$stat_plot <- renderPlot({
    x <- values()
    par(mar = c(4.4, 4.7, 2, 1), bg = "white")
    if (input$stat_chart == "box") {
      boxplot(x, horizontal = TRUE, col = "#dfe6ff", border = "#4358df",
              xlab = "Значение", main = "", pch = 19)
      grid(nx = NULL, ny = NA, col = "#e7eaf2")
      abline(v = mean(x), col = "#109c7d", lwd = 2, lty = 2)
      legend("topright", legend = "Среднее", col = "#109c7d",
             lty = 2, bty = "n")
    } else {
      hist(x, breaks = input$stat_bins, col = "#cad6ff", border = "white",
           xlab = "Значение", ylab = "Частота", main = "")
      abline(v = mean(x), col = "#109c7d", lwd = 2)
      abline(v = median(x), col = "#8454c8", lwd = 2, lty = 2)
      legend("topright", legend = c("Среднее", "Медиана"),
             col = c("#109c7d", "#8454c8"), lty = c(1, 2), bty = "n")
    }
  }, res = 100)
}

shinyApp(ui = ui, server = server)
