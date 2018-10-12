package com.example.lakshmanan.propexampl;

import android.content.Intent;
import android.support.v7.app.AppCompatActivity;
import android.os.Bundle;
import android.util.Log;
import android.widget.ListView;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public class ListViewActivity extends AppCompatActivity {

    ListView list;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_list_view);
        ListViewActivity ListActivityInstance = this;

        Intent intent = getIntent();
        String JSONString = intent.getStringExtra("JSON");
//        Log.d("In the new Intent is: ", JSONString);

        try {
            JSONArray array = new JSONArray(JSONString);
            String[] prices = getArray(array, "price_formatted");
            String[] imageUrls = getArray(array, "img_url");
            String[] titles = getArray(array, "title");

            CustomListAdapter adapter=new CustomListAdapter(ListActivityInstance, prices, imageUrls, titles);
            list=(ListView)findViewById(R.id.list);
            list.setAdapter(adapter);

        } catch (JSONException e) {
            e.printStackTrace();
        }
    }

    protected String[] getArray(JSONArray arr, String key) {
        String[] str = new String[arr.length()];
        try {
            for (int i = 0; i < arr.length(); i++) {
                JSONObject json = arr.getJSONObject(i);
                str[i] = json.getString(key);
                Log.d(key, str[i]);
            }
        } catch (JSONException e) {
            e.printStackTrace();
        }
        return str;
    }
}
