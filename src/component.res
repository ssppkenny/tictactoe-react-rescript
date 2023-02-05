%%raw("require('./styles.css')")

open Belt

open Tictactoe

let reducer = (state, action) => {
  update(state, action)
}

@react.component
let make = () => {
  let (state, dispatch) = React.useReducer(reducer, state)
  let row = (position: string, values: array<int>, row_number: int) => {
    let v = x => {
      if x == 1 {
        "o"
      } else if x == 2 {
        "x"
      } else {
        ""
      }
    }
    let v1 = React.string(v(values->Array.getUnsafe(0)))
    let v2 = React.string(v(values->Array.getUnsafe(1)))
    let v3 = React.string(v(values->Array.getUnsafe(2)))
    <div className="row">
      <div className={`left_${position}`} onClick={_ => Coords(row_number, 0)->dispatch}>
        <div className={cellStyle(row_number, 0, state, values->Array.getUnsafe(0))}> {v1} </div>
      </div>
      <div className={`center_${position}`} onClick={_ => Coords(row_number, 1)->dispatch}>
        <div className={cellStyle(row_number, 1, state, values->Array.getUnsafe(1))}> {v2} </div>
      </div>
      <div className={`right_${position}`} onClick={_ => Coords(row_number, 2)->dispatch}>
        <div className={cellStyle(row_number, 2, state, values->Array.getUnsafe(2))}> {v3} </div>
      </div>
    </div>
  }

  let board = state.board
  let positions = ["upper", "middle", "lower"]
  let elements = React.array(
    positions->Array.mapWithIndex((i, x) => {
      let r = board->Array.getUnsafe(i)
      row(x, r, i)
    }),
  )
  <div id="content">
    <div className="wrapper">
      <div className="field" />
      {elements}
    </div>
    <button className="button" onClick={_ => Restart->dispatch}> {"Restart"->React.string} </button>
  </div>
}
