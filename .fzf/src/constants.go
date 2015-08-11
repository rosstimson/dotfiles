package fzf

import (
	"time"

	"github.com/junegunn/fzf/src/util"
)

const (
	// Current version
	version = "0.10.2"

	// Core
	coordinatorDelayMax  time.Duration = 100 * time.Millisecond
	coordinatorDelayStep time.Duration = 10 * time.Millisecond

	// Reader
	defaultCommand = `find . -path '*/\.*' -prune -o -type f -print -o -type l -print 2> /dev/null | sed s/^..//`

	// Terminal
	initialDelay    = 100 * time.Millisecond
	spinnerDuration = 200 * time.Millisecond

	// Matcher
	progressMinDuration = 200 * time.Millisecond

	// Capacity of each chunk
	chunkSize int = 100

	// Do not cache results of low selectivity queries
	queryCacheMax int = chunkSize / 5

	// Not to cache mergers with large lists
	mergerCacheMax int = 100000

	// History
	defaultHistoryMax int = 1000
)

// fzf events
const (
	EvtReadNew util.EventType = iota
	EvtReadFin
	EvtSearchNew
	EvtSearchProgress
	EvtSearchFin
	EvtHeader
	EvtClose
)
