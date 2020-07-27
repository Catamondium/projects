class Diff {
	static states = {
        CORRECT: 0,
        BPOS: 1,
        INCORRECT: 2
    };
    
	constructor(val, state) {
		if (!Object.values(Diff.states).includes(state)) {throw "Invalid state"};
		this.value = val;
		this.state = state;
	}
    
    /*get color() { // implemented in styling
        switch(this.state) {
            case Diff.states.CORRECT:
                return 'green';
            case Diff.states.BPOS:
                return 'yellow';
            case Diff.states.INCORRECT:
                return 'red';
            default:
                throw 'Unreachable';
        }
    }*/
    
    render() {
        return this.state;
    }
};


function newAnswer() {
	let res = document.getElementById('guessbox');
	// just encase input 'type' is ignored
	let ans = res.value.toString().replace(/\D/.r, '');
	ans = ans.padStart(4, '0');
	if (ans.length > 4 || !unique(ans)) {
		res.value = '';
		return;
	}
    
    let display = document.getElementById('display');
    let diffs = compare(ans, state.goal);
    let row = display.insertRow(display.rows.length);
    genRow(row, diffs);
    
    let status = document.getElementById('status');
    if (!correct(diffs)) {
        state.attempts--;
        if (state.attempts >= 0) {
            status.value = `Remaining Attempts: ${state.attempts}`;
        } else {
            status.value = "FAILED";
            document.getElementById('answer').disabled = true;
        }
    } else {
        document.getElementById('answer').disabled = true;
        status.value = "SUCCESS";

    }
}

function genRow(row, diffs) {
    for (i in diffs) {
        let cell = row.insertCell(i);
        cell.innerHTML = diffs[i].value;
        cell.classList.add(`S${diffs[i].state}`);
        
    }
}

function correct(diffs) {
    for (diff of diffs) {
        if (diff.state != Diff.states.CORRECT) {
            return false;
        }
    }
    return true;
}

function compare(ans, goal) {
	ret = new Array();
	
	for (i in ans) {
		if (ans[i] == goal[i]){
            ret.push(new Diff(ans[i], Diff.states.CORRECT));
		} else if (goal.includes(ans[i])) {
			ret.push(new Diff(ans[i], Diff.states.BPOS));
		} else {
			ret.push(new Diff(ans[i], Diff.states.INCORRECT));
		}
	}
	
	return ret;
}

function unique(ans) {
	for (d of ans) {
		if(ans.indexOf(d) != ans.lastIndexOf(d)) {
			return false;
		}
	}
	return true;
}

function genGoal() {
	digits = new Array();
	
	while (digits.length != 4) {
		let n = Math.floor(Math.random() * 10)
		if (!digits.includes(n)) {
			digits.push(n);
		}
	}
	
	return digits.join('');
}

var state = undefined;
function rstState() {
    state = {
        attempts: 7,
        goal: genGoal()
    };
    let status = document.getElementById('status');
    document.getElementById('display').innerHTML = '';
    status.value = `Remaining Attempts: ${state.attempts}`;
    document.getElementById('guessbox').value = '';
    document.getElementById('answer').disabled = true;
}
