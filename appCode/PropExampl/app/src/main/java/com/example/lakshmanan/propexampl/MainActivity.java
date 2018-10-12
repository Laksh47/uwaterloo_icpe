package com.example.lakshmanan.propexampl;

import android.content.Intent;
import android.support.v7.app.AppCompatActivity;
import android.os.Bundle;
import android.util.Log;
import android.view.KeyEvent;
import android.view.View;
import android.view.inputmethod.EditorInfo;
import android.widget.Button;
import android.widget.EditText;
import android.widget.TextView;

import com.android.volley.Request;
import com.android.volley.RequestQueue;
import com.android.volley.Response;
import com.android.volley.VolleyError;
import com.android.volley.toolbox.JsonObjectRequest;
import com.android.volley.toolbox.StringRequest;
import com.android.volley.toolbox.Volley;

import org.json.JSONException;
import org.json.JSONObject;

public class MainActivity extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        final Button button = findViewById(R.id.button_go);
        final EditText eText   = (EditText)findViewById(R.id.editText);

        eText.setOnEditorActionListener(new TextView.OnEditorActionListener() {
            public boolean onEditorAction(TextView v, int actionId, KeyEvent event) {
                if ((event != null && (event.getKeyCode() == KeyEvent.KEYCODE_ENTER)) || (actionId == EditorInfo.IME_ACTION_DONE)) {
                    Log.d("Android","Enter pressed");
                    makeRequest(eText.getText().toString());
                }
                return false;
            }
        });

        eText.requestFocus();

        button.setOnClickListener(new View.OnClickListener() {
            public void onClick(View v) {
                Log.d("Text entered:", eText.getText().toString());
                makeRequest(eText.getText().toString());
            }
        });
    }

    protected String urlForQueryAndPage(String queryString) {
        return "https://api.nestoria.co.uk/api?country=uk&pretty=1&encoding=json&listing_type=buy&action=search_listings&page=1&place_name=" + queryString;
    }

    protected void makeRequest(String place) {
        // Instantiate the RequestQueue.
        RequestQueue queue = Volley.newRequestQueue(this);
        String url = urlForQueryAndPage(place);

        JsonObjectRequest jsObjRequest = new JsonObjectRequest(Request.Method.GET, url, null,
                new Response.Listener<JSONObject>() {
                    @Override
                    public void onResponse(JSONObject response) {
                        try {
                            Intent intent = new Intent(MainActivity.this, ListViewActivity.class);
                            intent.putExtra("JSON", response.getJSONObject("response").getJSONArray("listings").toString());
                            startActivity(intent);
                            //Log.d("Response is: ", response.getJSONObject("response").getJSONArray("listings").toString());
                        }
                        catch (JSONException e) {
                            e.printStackTrace();
                        }
                    }
                }, new Response.ErrorListener() {
            @Override
            public void onErrorResponse(VolleyError error) {
                Log.d("That didn't work!", "Poor request");
            }
        });
// Add the request to the RequestQueue.
        queue.add(jsObjRequest);
    }

}
