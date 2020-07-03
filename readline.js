function* main() {
    const arr = (yield).split(' ').map(v => parseInt(v))
    const tar = parseInt(yield)
    console.log(`${arr} ${tar}`)
}

const m = main()
const w = l => m.next(l).done && process.exit()
let b = ''
w()
process.stdin
    .setEncoding('utf8')
    .on('data', d => {
        const ls = d.split('\n')
        ls[0] = b + ls[0]
        b = ls.pop()
        ls.forEach(w)
    })
    .on('end', () => m.throw(new Error('EOF')))
