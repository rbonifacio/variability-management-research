package com.google.android.game2d.api.sprite;

import java.util.ArrayList;
import java.util.List;

public class ActionRecorder {

	public enum ACTION_CODE {LEFT, RIGHT, UP, DOWN};
	
	private static ActionRecorder instance; 
	private List<ACTION_CODE> actionList;
	
	public static ActionRecorder instance() {
		if (instance == null) {
			instance = new ActionRecorder();
		}
		return instance;
	}
	
	private ActionRecorder() {
		actionList = new ArrayList<ACTION_CODE>();
	}
	
	public void reset() {
		actionList.clear();
	}
	
	public void addAction(ACTION_CODE action) {
		actionList.add(action);
	}
	
	public ACTION_CODE[] getActions() {
		ACTION_CODE actions[] = new ACTION_CODE[actionList.size()];
		int i = 0;
		for (ACTION_CODE action_code : actionList) {
			actions[i++] = action_code;
		}
		return actions;
	}
}
