# CoreTCM

基于 R Shiny 的中药处方数据分析平台，提供中文与英文界面。

支持处方清洗与药名标准化、临床特征提取、频次与关联规则挖掘、
聚类分析、基于社区结构的核心配伍挖掘、核心配伍特征分析、
方证关联及候选药物推荐，以及多方法结果比较。

平台输出不构成医疗建议、诊断或处方。软件按原样提供，使用者应独立验证分析结果；
无担保及责任限制的完整说明见 [DISCLAIMER.md](DISCLAIMER.md) 和软件许可证。

## 安装与运行

本次发布副本在 Windows、R 4.4.2 环境下核查。建议使用 R 4.4.2 或更新版本。
R 包未锁定版本；安装脚本从 CRAN 安装当前环境兼容的可用版本。

下载或克隆后，将工作目录设为本 README 所在目录，在 R / RStudio 中运行：

```r
source("scripts/install_dependencies.R", encoding = "UTF-8")
source("scripts/check_environment.R", encoding = "UTF-8")
source("scripts/run_app.R", encoding = "UTF-8")
```

也可以在终端中运行：

```sh
Rscript --vanilla scripts/install_dependencies.R
Rscript --vanilla scripts/check_environment.R
Rscript --vanilla scripts/run_app.R
```

首次安装需要网络。部分 R 包如无适用的二进制版本，可能需要本机编译工具。
安装脚本会在包缺失时明确报错；环境检查会列出缺失依赖。
终端启动后，在浏览器打开控制台显示的本地地址。启动脚本在 Windows 上设置 UTF-8 字符环境，
避免终端遗留的区域设置影响中文路径和源码读取。

## 示例数据与操作

| 文件 | 用途 |
| --- | --- |
| `data/example_formula_only.csv` | 7 条处方导入示例，含剂量与多种分隔符 |
| `data/example_formula_clinical.csv` | 7 条含临床字段的导入示例 |
| `examples/synthetic/edema_sim_sample_1000.csv` | 1,000 条人工模拟处方，用于演示完整分析流程 |
| `examples/synthetic/syndrome_mapping.csv` | 模拟数据的原始证型到标准证型映射 |
| `examples/synthetic/syndrome_bingwei_bingxing.csv` | 模拟数据的标准证型到病位、病性映射 |
| `data/template_*.csv` | 自定义药名、药性及证型映射表的格式示例 |

小示例：进入“数据管理与中药名称标准化”，上传临床示例，点击“读取文件”，
选择 `formula` 列并运行清洗；再进入临床特征提取和后续分析。

完整演示：上传 1,000 条模拟数据，选择 `herb_medicine` 为处方列，运行清洗。
在临床特征模块上传证型映射表，病位和病性可选择“使用数据中的列”；
若要演示映射提取，上传病位/病性映射表并选择“查表映射”。
随后依次运行描述性分析、社区挖掘、配伍特征分析、方证关联及方法比较。
清洗默认去重，因此后续分析条数可能少于原始的 1,000 条。

CSV 使用 UTF-8；平台同时尝试读取 GB18030 / GBK。
模拟样本的标识符、年龄、性别和处方均由生成脚本构造；样本不包含真实患者记录。
模拟数据仅用于软件演示，不能用于评价临床疗效或指导用药。
数据字段和字典说明见 [docs/DATA.md](docs/DATA.md)。

## 目录

```text
app.R                     Shiny 应用入口
global.R                  共享参数与依赖加载
R/                        M0–M6 模块和分析工具
www/                      样式与中英文界面资源
data/                     内置字典、导入示例及映射模板
examples/synthetic/       模拟数据、映射表与生成脚本
scripts/                  依赖安装和环境检查
tests/                    已有回归测试及视觉核查脚本
docs/                     数据与验证说明
```

## 验证

从仓库根目录运行已有测试：

```sh
Rscript --vanilla -e 'Sys.setlocale("LC_CTYPE", ".UTF-8"); source("tests/test_i18n.R", encoding = "UTF-8")'
Rscript --vanilla tests/test_dosage.R
Rscript --vanilla tests/test_clinical_visual_regressions.R
node tests/test_i18n.js
```

最后一项需要 Node.js，仅用于客户端测试，平台运行本身不需要 Node.js。
`tests/prepare_visual_report.R` 可生成完整分析流程的 HTML 核查报告；
生成目录 `tests/visual-report/` 已排除 Git 跟踪。
本次实际验证结果见 [docs/VALIDATION.md](docs/VALIDATION.md)。

## 许可

本项目的软件代码、文档和原创模拟示例采用 [MIT 许可证](LICENSE)，
允许使用、修改、再分发和商业使用，要求保留版权与许可声明。
软件按原样提供，无担保及责任限制见许可证全文；使用说明另见 [DISCLAIMER.md](DISCLAIMER.md)。

第三方软件与数据的权利归其各自权利人所有。`data/` 中两份内置 Excel 字典的原始来源和许可
尚待补充，不因本项目的软件 MIT 许可而自动获得重新授权；具体说明见 [docs/DATA.md](docs/DATA.md)。
